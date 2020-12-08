{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |Handles migration of DB schema and data in SQLite.
-- This is heavily inspired by <https://github.com/ameingast/postgresql-simple-migration postgressql-simple-migration>
-- package and it should be possible to factor out the commonalities in a single package
-- with specific drivers for various backends.
module Sensei.DB.SQLite.Migration where

import Control.Exception.Safe
import Control.Monad.Reader
import Data.Text (Text, pack)
import Database.SQLite.Simple
import Preface.Utils

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

initialMigration :: Connection -> IO MigrationResult
initialMigration cnx = do
  tblExists <- tableExists "schema_migrations" cnx
  unless tblExists $ execute_ cnx createMigrationTable
  pure MigrationSuccessful

data Migration
  = InitialMigration
  | Migration {name :: Text, apply :: Query, rollback :: Query}

instance Hashable Migration where
  hashOf InitialMigration = hashOf ("InitialMigration" :: Text)
  hashOf Migration {..} = hashOf name <> hashOf (fromQuery apply) <> hashOf (fromQuery rollback)

createLog :: Migration
createLog =
  Migration
    "createLog"
    ( mconcat
        [ "create table if not exists event_log ",
          "( id integer primary key",
          ", timestamp text not null",
          ", version integer not null",
          ", flow_type text not null",
          ", flow_data text not null",
          ");"
        ]
    )
    "drop table event_log;"

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
  execute_ cnx apply
  execute cnx q (name, toText h)
  pure $ MigrationSuccessful

runMigrations :: Connection -> [Migration] -> IO MigrationResult
runMigrations cnx =
  foldM (runMigration cnx) MigrationSuccessful

migrateSQLiteDB :: FilePath -> IO ()
migrateSQLiteDB sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    migResult <- runMigrations cnx [InitialMigration, createLog]
    case migResult of
      MigrationSuccessful -> pure ()
      MigrationFailed err -> throwIO $ SQLiteDBError err

data SQLiteDBError = SQLiteDBError Text
  deriving (Eq, Show)

instance Exception SQLiteDBError
