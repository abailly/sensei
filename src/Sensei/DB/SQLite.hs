{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Implementation of flows DB using <https://www.sqlite.org/index.html SQLite>
-- embedded DB
module Sensei.DB.SQLite where

import Control.Exception.Safe
import Control.Monad.Reader
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.LocalTime
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Preface.Utils
import Sensei.API
import Sensei.DB
import Sensei.DB.File
import System.Directory
import System.IO

data SQLiteConfig = SQLiteConfig
  { storagePath :: FilePath,
    configDir :: FilePath
  }

newtype SQLiteDB a = SQLiteDB {unSQLite :: ReaderT SQLiteConfig IO a}
  deriving (Functor, Applicative, Monad, MonadReader SQLiteConfig, MonadIO)

instance DB SQLiteDB where
  initLogStorage = initSQLiteDB
  writeTrace t = SQLiteDB $ asks storagePath >>= liftIO . writeTraceSQL t
  writeFlow f = SQLiteDB $ asks storagePath >>= liftIO . writeFlowSQL f
  writeProfile u = SQLiteDB $ (asks configDir >>= liftIO . writeProfileFile u)
  readNotes u = SQLiteDB $ asks storagePath >>= liftIO . readNotesSQL u
  readViews u = SQLiteDB $ asks storagePath >>= liftIO . readViewsSQL u
  readCommands u = SQLiteDB $ asks storagePath >>= liftIO . readCommandsSQL u
  readProfile = SQLiteDB $ (asks configDir >>= liftIO . readProfileFile)

runSQLite ::
  FilePath -> SQLiteDB a -> IO a
runSQLite dbFile act =
  unSQLite (initSQLiteDB >> act) `runReaderT` SQLiteConfig dbFile "."

initSQLiteDB :: SQLiteDB ()
initSQLiteDB = do
  sqliteFile <- asks storagePath
  sqliteFileExists <- liftIO $ doesFileExist sqliteFile
  unless sqliteFileExists $ liftIO $ openFile sqliteFile WriteMode >>= hClose
  liftIO $ migrateSQLiteDB sqliteFile

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

instance ToRow Flow where
  toRow Flow {..} =
    let ts = toField (_flowStart _flowState)
        payload = toField $ decodeUtf8' $ LBS.toStrict $ encode _flowState
     in [ts, SQLInteger (fromIntegral currentVersion), toField $ toText _flowType, payload]

instance FromRow Flow where
  fromRow = do
    _ts :: UTCTime <- field
    ver <- fromInteger <$> field
    ty <- either (error . unpack) id . parseFlowType <$> field
    st <- either error id . eitherDecode . encodeUtf8 <$> field
    pure $ Flow ty st ver

instance ToRow Trace where
  toRow trace@Trace {..} =
    let ts = toField timestamp
        payload = toField $ decodeUtf8' $ LBS.toStrict $ encode trace
     in [ts, SQLInteger (fromIntegral currentVersion), toField ("__TRACE__" :: Text), payload]

instance FromRow Trace where
  fromRow = either error id . eitherDecode . encodeUtf8 <$> field

writeFlowSQL :: Flow -> FilePath -> IO ()
writeFlowSQL flow sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "insert into event_log (timestamp, version, flow_type, flow_data) values (?, ?, ?, ?)"
    execute cnx q flow

writeTraceSQL :: Trace -> FilePath -> IO ()
writeTraceSQL trace sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "insert into event_log (timestamp, version, flow_type, flow_data) values (?, ?, ?, ?)"
    execute cnx q trace

readNotesSQL :: UserProfile -> FilePath -> IO [(LocalTime, Text)]
readNotesSQL UserProfile {..} sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select timestamp, version, flow_type, flow_data from event_log where flow_type = 'Note' order by timestamp"
    notesFlow <- query_ cnx q
    pure $ foldr (notesViewBuilder userName userTimezone) [] notesFlow

readViewsSQL :: UserProfile -> FilePath -> IO [FlowView]
readViewsSQL UserProfile {..} sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select timestamp, version, flow_type, flow_data from event_log where flow_type != 'Note' and flow_type != '__TRACE__' order by timestamp"
    flows <- query_ cnx q
    pure $ foldr (flowViewBuilder userName userTimezone userEndOfDay) [] flows

readCommandsSQL :: UserProfile -> FilePath -> IO [CommandView]
readCommandsSQL UserProfile {..} sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select flow_data from event_log where flow_type = '__TRACE__' order by timestamp"
    traces <- query_ cnx q
    pure $ foldr (commandViewBuilder userTimezone) [] traces
