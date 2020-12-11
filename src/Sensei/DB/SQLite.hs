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
module Sensei.DB.SQLite (runDB, getDataFile, migrateFileDB, SQLiteDB) where

import Control.Exception (throwIO)
import Control.Exception.Safe (IOException, try)
import Control.Monad.Reader
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.LocalTime
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Preface.Utils (decodeUtf8', toText)
import Sensei.API
import Sensei.DB
import Sensei.DB.File (readProfileFile, writeProfileFile)
import qualified Sensei.DB.File as File
import Sensei.DB.SQLite.Migration
import Sensei.IO
import System.Directory
import System.FilePath ((<.>), (</>))
import System.IO

-- | The configuration for DB engine.
data SQLiteConfig = SQLiteConfig
  { -- | Path to SQLite database file
    storagePath :: FilePath,
    -- | Directory where user-based configuration is stored. Note that
    --  we reuse the file-based configuration mechanism from `Sensei.DB.File`, it
    --  would probably be better to store everything into the SQLite DB file.
    configDir :: FilePath
  }

-- | Simple monad stack to run SQLite actions within the context of a `SQLiteConfig`.
newtype SQLiteDB a = SQLiteDB {unSQLite :: ReaderT SQLiteConfig IO a}
  deriving (Functor, Applicative, Monad, MonadReader SQLiteConfig, MonadIO)

-- | Runs a DB action using given DB file and configuration directory.
runDB ::
  FilePath -> FilePath -> SQLiteDB a -> IO a
runDB dbFile configDir act =
  unSQLite act `runReaderT` SQLiteConfig dbFile configDir

getDataFile :: FilePath -> IO FilePath
getDataFile configDir = do
  oldLog <- getDataDirectory >>= pure . (</> "sensei.log")
  newLog <- getDataDirectory >>= pure . (</> "sensei.sqlite")
  existOldLog <- doesFileExist oldLog
  if existOldLog
    then migrateOldLog oldLog newLog
    else pure newLog
  where
    migrateOldLog :: FilePath -> FilePath -> IO FilePath
    migrateOldLog old new = do
      res <- migrateFileDB old configDir
      case res of
        Left err -> throwIO $ userError $ "failed to migrate File DB, aborting " <> unpack err
        Right _ -> renameFile old new >> pure new

-- | Migrate an existing File-based event log to a SQLite-based one.
--  The old database is preserved and copied to a new File with same name
--  but suffixed `.old`.
--  Returns either an error description or the (absolute) path to the old
--  file-based event log.
migrateFileDB ::
  FilePath -> FilePath -> IO (Either Text FilePath)
migrateFileDB dbFile configDir = do
  let oldLog = dbFile <.> "old"
  renameFile dbFile oldLog
  evs <- try $ File.runDB oldLog configDir File.readAll
  case evs of
    Left (e :: IOException) -> pure $ Left (pack $ show e)
    Right events -> runDB dbFile configDir $ do
      initSQLiteDB
      writeAll events
      pure $ Right oldLog

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

instance FromRow Event where
  fromRow = do
    ty <- field
    case ty of
      "__TRACE__" -> do
        tr <- T . either error id . eitherDecode . encodeUtf8 <$> field
        _ver :: Integer <- field -- Discard version
        pure tr
      _ -> do
        let flowType = parseFlowType ty
        case flowType of
          Left err -> error err
          Right t -> do
            st <- either error id . eitherDecode . encodeUtf8 <$> field
            v <- fromInteger <$> field
            pure $ F (Flow t st v)

-- * DB Implementation

instance DB SQLiteDB where
  initLogStorage = initSQLiteDB
  writeTrace t = SQLiteDB $ asks storagePath >>= liftIO . writeTraceSQL t
  writeFlow f = SQLiteDB $ asks storagePath >>= liftIO . writeFlowSQL f
  writeProfile u = SQLiteDB $ (asks configDir >>= liftIO . writeProfileFile u)
  readEvents u = SQLiteDB $ (asks storagePath >>= liftIO . readEventsSQL u)
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

writeAll ::
  [Event] -> SQLiteDB ()
writeAll events = SQLiteDB $ do
  file <- asks storagePath
  liftIO $
    withConnection file $ \cnx -> do
      let storeEvent (F flow) = insert flow cnx
          storeEvent (T trace) = insert trace cnx
      forM_ events storeEvent

writeFlowSQL :: Flow -> FilePath -> IO ()
writeFlowSQL flow sqliteFile =
  withConnection sqliteFile $ insert flow

writeTraceSQL :: Trace -> FilePath -> IO ()
writeTraceSQL trace sqliteFile =
  withConnection sqliteFile $ insert trace

insert ::
  ToRow q => q -> Connection -> IO ()
insert event cnx = do
  let q = "insert into event_log (timestamp, version, flow_type, flow_data) values (?, ?, ?, ?)"
  execute cnx q event

readEventsSQL :: UserProfile -> FilePath -> IO [Event]
readEventsSQL _ sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select flow_type, flow_data, version from event_log order by timestamp desc"
    query_ cnx q

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
    pure $ reverse $ foldl (flip $ flowViewBuilder userName userTimezone userEndOfDay) [] flows

readCommandsSQL :: UserProfile -> FilePath -> IO [CommandView]
readCommandsSQL UserProfile {..} sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select flow_data from event_log where flow_type = '__TRACE__' order by timestamp"
    traces <- query_ cnx q
    pure $ foldr (commandViewBuilder userTimezone) [] traces
