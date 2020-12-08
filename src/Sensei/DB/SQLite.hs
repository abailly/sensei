{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Implementation of flows DB using <https://www.sqlite.org/index.html SQLite>
-- embedded DB.
--
-- The DB is expected to have the following schema:
-- @@
-- CREATE TABLE schema_migrations ( id integer primary key,
--                                  filename text not null,
--                                  checksum text not null,
--                                  executed_at integer default (strftime('%s','now')) not null);
-- CREATE TABLE event_log ( id integer primary key,
--                          timestamp text not null,
--                          version integer not null,
--                          flow_type text not null,
--                          flow_data text not null);
-- @@
-- The actual data is stored in the `flow_data` field as JSON.
module Sensei.DB.SQLite where

import Control.Monad.Reader
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.LocalTime
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Preface.Utils
import Sensei.API
import Sensei.DB
import Sensei.DB.File
import Sensei.DB.SQLite.Migration
import System.Directory
import System.IO

-- |The configuration for DB engine.
data SQLiteConfig = SQLiteConfig
  { storagePath :: FilePath,
    -- ^Path to SQLite database file
    configDir :: FilePath
    -- ^Directory where user-based configuration is stored. Note that
    -- we reuse the file-based configuration mechanism from `Sensei.DB.File`, it
    -- would probably be better to store everything into the SQLite DB file.
  }

-- |Simple monad stack to run SQLite actions within the context of a `SQLiteConfig`.
newtype SQLiteDB a = SQLiteDB {unSQLite :: ReaderT SQLiteConfig IO a}
  deriving (Functor, Applicative, Monad, MonadReader SQLiteConfig, MonadIO)

-- |Runs a DB action using given DB file and configuration directory.
-- Note this runner automatically initiliazes the SQLite DB so there's
-- no need to do it explicitly.
runSQLite ::
  FilePath -> FilePath -> SQLiteDB a -> IO a
runSQLite dbFile configDir act =
  unSQLite (initSQLiteDB >> act) `runReaderT` SQLiteConfig dbFile configDir

-- * Instances
-- Various instances for `Flow` and `Trace` to be abel to read and write from the
-- @event_log@ table that contains all events.
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

-- * DB Implementation

instance DB SQLiteDB where
  initLogStorage = initSQLiteDB
  writeTrace t = SQLiteDB $ asks storagePath >>= liftIO . writeTraceSQL t
  writeFlow f = SQLiteDB $ asks storagePath >>= liftIO . writeFlowSQL f
  writeProfile u = SQLiteDB $ (asks configDir >>= liftIO . writeProfileFile u)
  readNotes u = SQLiteDB $ asks storagePath >>= liftIO . readNotesSQL u
  readViews u = SQLiteDB $ asks storagePath >>= liftIO . readViewsSQL u
  readCommands u = SQLiteDB $ asks storagePath >>= liftIO . readCommandsSQL u
  readProfile = SQLiteDB $ (asks configDir >>= liftIO . readProfileFile)

-- initializes the DB file
-- if the file does not exist, it is created
initSQLiteDB :: SQLiteDB ()
initSQLiteDB = do
  sqliteFile <- asks storagePath
  sqliteFileExists <- liftIO $ doesFileExist sqliteFile
  unless sqliteFileExists $ liftIO $ openFile sqliteFile WriteMode >>= hClose
  liftIO $ migrateSQLiteDB sqliteFile

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
