{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Handles migration of DB schema and data in SQLite.
--  This is heavily inspired by <https://github.com/ameingast/postgresql-simple-migration postgressql-simple-migration>
--  package and it should be possible to factor out the commonalities in a single package
--  with specific drivers for various backends.
module Sensei.DB.SQLite.Migration where

import Control.Exception.Safe
import Control.Monad.Reader
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Database.SQLite.Simple
import Preface.Utils
import Preface.Log (LoggerEnv, logInfo)
import GHC.Generics(Generic)
import Data.Aeson(ToJSON, FromJSON)

-- Orphans
deriving newtype instance ToJSON Query
deriving newtype instance FromJSON Query

createMigrationTable :: Query
createMigrationTable =
  mconcat
    [ "create table if not exists schema_migrations ",
      "( id integer primary key",
      ", filename text not null",
      ", checksum text not null",
      ", executed_at integer default (strftime('%s','now')) not null",
      ");"
    ]

tableExists :: Text -> Connection -> IO Bool
tableExists tblName cnx = do
  res :: [[Text]] <- query cnx "SELECT name FROM sqlite_master WHERE type='table' AND name=?;" (Only tblName)
  case res of
    (_ : _) -> pure True
    [] -> pure False

data MigrationResult
  = MigrationSuccessful
  | MigrationFailed {reason :: Text}
  deriving (Eq, Show, Read,Generic, ToJSON, FromJSON)

initialMigration :: Connection -> IO MigrationResult
initialMigration cnx = do
  tblExists <- tableExists "schema_migrations" cnx
  unless tblExists $ execute_ cnx createMigrationTable
  pure MigrationSuccessful

data Migration
  = InitialMigration
  | Migration {name :: Text, apply :: [Query], rollback :: Query}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Hashable Migration where
  hashOf InitialMigration = hashOf ("InitialMigration" :: Text)
  hashOf Migration {..} = hashOf name <> hashOf (Text.concat $ fmap fromQuery apply) <> hashOf (fromQuery rollback)

createLog :: Migration
createLog =
  Migration
    "createLog"
    [ mconcat
        [ "create table if not exists event_log ",
          "( id integer primary key",
          ", timestamp text not null",
          ", version integer not null",
          ", flow_type text not null",
          ", flow_data text not null",
          ");"
        ]
    ]
    "drop table event_log;"

createConfig :: Migration
createConfig =
  Migration
    "createLog"
    [ mconcat
        [ "create table if not exists config_log ",
          "( id integer primary key",
          ", timestamp text not null",
          ", version integer not null",
          ", user text not null",
          ", key text not null",
          ", value text not null",
          ");"
        ]
    ]
    "drop table config_log;"

createNotesSearch :: Migration
createNotesSearch =
  Migration
    "createNotesSearch"
    ["create virtual table notes_search using fts5(id, note);"]
    "drop table notes_search;"

populateSearch :: Migration
populateSearch =
  Migration
    "populateSearch"
    ["insert into notes_search select id,json_extract(flow_data, '$._flowNote') from event_log where flow_type = 'Note';"]
    "delete from notes_search;"

createUsers :: Migration
createUsers =
  Migration
    "createUsersTable"
    [ mconcat
        [ "create table if not exists users ",
          "( id integer primary key",
          ", uid text not null",
          ", user text not null",
          ", profile text not null",
          ");"
        ]
    ]
    "drop table users;"

addUserInEventLog :: Migration
addUserInEventLog =
  Migration
    "addUserInEventLog"
    [ mconcat
        [ "alter table event_log  ",
          " add column user text not null default '';"
        ]
    ]
    ( mconcat
        [ "alter table event_log  ",
          " drop column user;"
        ]
    )

updateUserInEventLog :: Migration
updateUserInEventLog =
  Migration
    "updateUserInEventLog"
        [ "create table tmp_users ( id integer primary key, user text not null);",
          "insert into tmp_users select l.id as id,  j.value  as user from event_log as l, json_each(l.flow_data,'$._flowUser') as j;",
          "insert into tmp_users select l.id as id,  j.value  as user from event_log as l, json_each(l.flow_data,'$.traceUser') as j;",
          "update event_log set user = (select user from tmp_users where tmp_users.id = id);",
          "drop table tmp_users;"
        ]
    ""

runMigration ::
  Connection -> MigrationResult -> Migration -> IO MigrationResult
runMigration _ f@MigrationFailed {} _ = pure f -- shortcut execution when failing
runMigration cnx _ m = do
  hasRun <- checkHasRun m cnx
  ( if not hasRun
      then applyMigration m cnx
      else pure MigrationSuccessful
    )
    `catches` [ Handler $ \(e :: FormatError) -> pure $ MigrationFailed (pack $ show e),
                Handler $ \(e :: ResultError) -> pure $ MigrationFailed (pack $ show e),
                Handler $ \(e :: SQLError) -> pure $ MigrationFailed (pack $ show e)
              ]

checkHasRun :: Migration -> Connection -> IO Bool
checkHasRun InitialMigration cnx = tableExists "schema_migrations" cnx
checkHasRun m@Migration {..} cnx = do
  let h = hashOf m
      q = "select id from schema_migrations where filename = ? and checksum = ?"
  migrationExists <- tableExists "schema_migrations" cnx
  res :: [Only Int] <- query cnx q (name :: Text, toText h)
  pure $ migrationExists && length res == 1

applyMigration :: Migration -> Connection -> IO MigrationResult
applyMigration InitialMigration cnx = initialMigration cnx
applyMigration m@Migration {..} cnx = do
  let h = hashOf m
      q = "insert into schema_migrations (filename, checksum) values (?,?)"
  withExclusiveTransaction cnx $ do
    mapM_ (execute_ cnx) apply
    execute cnx q (name, toText h)
  pure MigrationSuccessful

data RunMigration = RunningMigration { migration :: Migration }
  | RanMigration { migration :: Migration, result :: MigrationResult }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

runMigrations :: LoggerEnv -> Connection -> [Migration] -> IO MigrationResult
runMigrations logger cnx =
  foldM doRunMigration MigrationSuccessful
  where
    doRunMigration result migration = do
      logInfo logger (RunningMigration migration)
      result' <- runMigration cnx result migration
      logInfo logger (RanMigration migration result')
      pure result'
