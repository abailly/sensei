{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
-- CREATE TABLE config_log ( id integer primary key,
--                           timestamp text not null,
--                           version integer not null,
--                           user text not null,
--                           key text not null,
--                           value text not null);
-- @@
-- The actual data is stored in the `flow_data` field as JSON.
module Sensei.DB.SQLite (runDB, getDataFile, migrateFileDB, SQLiteDB, SQLiteDBError(..)) where

import Control.Exception (throwIO)
import Control.Exception.Safe (MonadCatch, throwM, Exception, IOException, MonadThrow, try)
import Control.Monad.Reader
import Data.Aeson (eitherDecode, encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import qualified Data.Time as Time
import Data.Time.LocalTime
import Database.SQLite.Simple hiding (execute, execute_, query, query_)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.ToField
import GHC.Stack (HasCallStack)
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
import Control.Exception.Safe (catches)
import Control.Exception.Safe (Handler(Handler))

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
  deriving (Functor, Applicative, Monad, MonadReader SQLiteConfig, MonadThrow, MonadCatch, MonadIO)

-- | Runs a DB action using given DB file and configuration directory.
runDB ::
  HasCallStack =>
  FilePath ->
  FilePath ->
  SQLiteDB a ->
  IO a
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
  HasCallStack =>
  FilePath ->
  FilePath ->
  IO (Either Text FilePath)
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

instance ToRow Event where
  toRow e =
    let ts = toField (eventTimestamp e)
        payload = toField $ decodeUtf8' $ LBS.toStrict $ encode e
        typ = typeOf e
     in [ts, SQLInteger (fromIntegral currentVersion), toField (typ :: Text), payload]

typeOf :: Event -> Text
typeOf EventTrace {} = "__TRACE__"
typeOf (EventFlow Flow {_flowType}) = toText _flowType
typeOf EventNote {} = "Note"

instance FromRow Event where
  fromRow = do
    _ts :: UTCTime <- field
    ver :: Integer <- field
    ty :: Text <- field
    if ver >= 5
      then either error id . eitherDecode . encodeUtf8 <$> field
      else case ty of
        "__TRACE__" -> decodeTracev4 <$> field
        "Note" -> decodeNotev4 <$> field
        _ -> decodeFlowv4 ty <$> field
    where
      decodeTracev4 txt =
        let val = eitherDecode $ encodeUtf8 txt
         in case val of
              Left err -> error err
              Right j -> case A.parse parseEventFromv4 j of
                A.Success t -> t
                A.Error f -> error f
      decodeNotev4 txt =
        let val = eitherDecode $ encodeUtf8 txt
         in case val of
              Left err -> error err
              Right j -> case A.parse parseNoteFromv4 j of
                A.Success t -> EventNote t
                A.Error f -> error f
      decodeFlowv4 ty txt =
        let val = eitherDecode $ encodeUtf8 txt
            maybetype = parseFlowType ty
         in case (val, maybetype) of
              (Left err, _) -> error err
              (_, Nothing) -> error $ "failed to decode flow type " <> unpack ty
              (Right j, Just ftype) -> case A.parse parseFlowFromv4 j of
                A.Success t -> EventFlow t {_flowType = ftype}
                A.Error f -> error f

data IdFlow = IdFlow {identifier :: Int64, event :: Event}

instance FromRow IdFlow where
  fromRow = IdFlow <$> field <*> fromRow

-- | This instance returns a number to be used as offset value in a query
instance ToField Reference where
  toField Latest = SQLInteger 0
  toField (Pos n) = SQLInteger (fromIntegral n)

-- * DB Implementation

-- ** Error Handling

data SQLiteDBError = SQLiteDBError Query Text
  deriving (Eq, Show)

instance Exception SQLiteDBError

-- |Repack errors thrown by `SQLite` engine into a `SQLiteDBError`.
handleErrors :: MonadCatch m => Query -> m a -> m a
handleErrors q m =
  m `catches` [ Handler $ \(e :: FormatError) -> throwM $ SQLiteDBError q (pack $ show e),
                Handler $ \(e :: ResultError) -> throwM $ SQLiteDBError q (pack $ show e),
                Handler $ \(e :: SQLError) -> throwM $ SQLiteDBError q (pack $ show e)
              ]

execute :: ToRow q => Connection -> Query -> q -> IO ()
execute cnx q args = handleErrors q $ SQLite.execute cnx q args

query :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO [r]
query cnx q args = handleErrors q $ SQLite.query cnx q args

query_ :: (FromRow r) => Connection -> Query -> IO [r]
query_ cnx q = handleErrors q $ SQLite.query_ cnx q

instance DB SQLiteDB where
  type DBError SQLiteDB = SQLiteDBError
  initLogStorage = initSQLiteDB
  setCurrentTime u ts = SQLiteDB $ asks storagePath >>= liftIO . setCurrentTimeSQL u ts
  getCurrentTime u = SQLiteDB $ asks storagePath >>= liftIO . getCurrentTimeSQL u
  writeEvent t = SQLiteDB $ asks storagePath >>= liftIO . writeEventSQL t
  updateLatestFlow ts = SQLiteDB $ asks storagePath >>= liftIO . updateLatestFlowSQL ts
  writeProfile u = SQLiteDB $ (asks configDir >>= liftIO . writeProfileFile u)
  readFlow u r = SQLiteDB $ (asks storagePath >>= liftIO . readFlowSQL u r)
  readEvents u p = SQLiteDB $ (asks storagePath >>= liftIO . readEventsSQL u p)
  readNotes u rge = SQLiteDB $ asks storagePath >>= liftIO . readNotesSQL u rge
  searchNotes u txt = SQLiteDB $ asks storagePath >>= liftIO . searchNotesSQL u txt
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

migrateSQLiteDB :: FilePath -> IO ()
migrateSQLiteDB sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    migResult <-
      runMigrations
        cnx
        [ InitialMigration,
          createLog,
          createConfig,
          createNotesSearch,
          populateSearch
        ]
    case migResult of
      MigrationSuccessful -> pure ()
      MigrationFailed err -> throwM $ SQLiteDBError "" err

setCurrentTimeSQL :: UserProfile -> UTCTime -> FilePath -> IO ()
setCurrentTimeSQL UserProfile {userName} newTime sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "insert into config_log(timestamp, version, user, key, value) values (?,?,?,?,?)"
    ts <- Time.getCurrentTime -- This is silly, isn't it?
    execute cnx q [toField ts, toField (fromIntegral currentVersion :: Integer), toField userName, toField ("currentTime" :: Text), toField newTime]

getCurrentTimeSQL :: UserProfile -> FilePath -> IO UTCTime
getCurrentTimeSQL UserProfile {userName} sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select value from config_log where user = ? and key = 'currentTime' order by timestamp desc limit 1"
    res <- query cnx q (Only userName)
    case res of
      ([ts] : _) -> pure ts
      _ -> Time.getCurrentTime

writeAll ::
  HasCallStack =>
  [Event] ->
  SQLiteDB ()
writeAll events = SQLiteDB $ do
  file <- asks storagePath
  liftIO $
    withConnection file $ \cnx -> forM_ events (`insert` cnx)

updateNotesIndex ::
  HasCallStack =>
  Int ->
  Event ->
  Connection ->
  IO ()
updateNotesIndex rid (EventNote NoteFlow {..}) cnx =
  let q = "insert into notes_search (id, note) values (?, ?)"
   in execute cnx q [toField rid, SQLText $ _noteContent]
updateNotesIndex _ _ _ = pure ()

writeEventSQL ::
  HasCallStack =>
  Event ->
  FilePath ->
  IO ()
writeEventSQL e sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    rid <- insert e cnx
    updateNotesIndex rid e cnx

insert ::
  HasCallStack =>
  ToRow q =>
  q ->
  Connection ->
  IO Int
insert event cnx = do
  let q = "insert into event_log (timestamp, version, flow_type, flow_data) values (?, ?, ?, ?)"
  execute cnx q event
  [[r]] <- query_ cnx "SELECT last_insert_rowid()"
  pure r

updateLatestFlowSQL ::
  HasCallStack =>
  NominalDiffTime ->
  FilePath ->
  IO Event
updateLatestFlowSQL diff sqliteFile =
  withConnection sqliteFile $ \cnx ->
    withTransaction cnx $ do
      let q = "select id, timestamp, version, flow_type, flow_data from event_log where flow_type != '__TRACE__' order by timestamp desc limit 1"
          u = "update event_log set timestamp = ?, flow_data = ? where id = ?"
      res <- query_ cnx q
      case res of
        (IdFlow {identifier, event = EventFlow f@Flow {_flowTimestamp}} : _) -> do
          let newTs = addUTCTime diff _flowTimestamp
              updatedFlow = EventFlow f {_flowTimestamp = newTs}
          execute cnx u [toField newTs, toField $ decodeUtf8' $ LBS.toStrict $ encode updatedFlow, toField identifier]
          pure updatedFlow
        (IdFlow {event} : _) -> pure event -- don't change other events
        [] -> error "no flows recorded"

readFlowSQL ::
  HasCallStack =>
  UserProfile ->
  Reference ->
  FilePath ->
  IO (Maybe Event)
readFlowSQL _ ref sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select timestamp, version, flow_type, flow_data from event_log where flow_type != '__TRACE__' order by timestamp desc limit 1 offset ?"
    res <- query cnx q (Only ref)
    case res of
      [flow] -> pure $ Just flow
      [] -> pure Nothing
      _ -> error "invalid query results"

readEventsSQL ::
  HasCallStack =>
  UserProfile ->
  Pagination ->
  FilePath ->
  IO EventsQueryResult
readEventsSQL _ Page {pageNumber, pageSize} sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select timestamp, version, flow_type, flow_data from event_log order by timestamp desc limit ? offset ?"
        count = "select count(*) from event_log"
    resultEvents <- query cnx q [SQLInteger $ fromIntegral pageSize, SQLInteger $ fromIntegral $ (pageNumber - 1) * pageSize]
    [[numEvents]] <- query_ cnx count
    let totalEvents = fromInteger numEvents
        eventsCount = fromIntegral $ length resultEvents
        startIndex = min ((pageNumber - 1) * pageSize) totalEvents
        endIndex = min (pageNumber * pageSize) totalEvents
    pure $ EventsQueryResult {..}

readNotesSQL ::
  HasCallStack =>
  UserProfile ->
  TimeRange ->
  FilePath ->
  IO [(LocalTime, Text)]
readNotesSQL UserProfile {..} TimeRange {..} sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select timestamp, version, flow_type, flow_data from event_log where flow_type = 'Note' and datetime(timestamp) between ? and ? order by timestamp"
    notesFlow <- query cnx q [rangeStart, rangeEnd]
    pure $ foldr (notesViewBuilder userName userTimezone) [] notesFlow

searchNotesSQL ::
  HasCallStack =>
  UserProfile ->
  Text ->
  FilePath ->
  IO [(LocalTime, Text)]
searchNotesSQL UserProfile {..} text sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select timestamp, note from event_log inner join notes_search on event_log.id = notes_search.id where notes_search match ?"
    notesFlow <- query cnx q [text]
    pure $ fmap (first $ utcToLocalTime userTimezone) notesFlow

readViewsSQL ::
  HasCallStack =>
  UserProfile ->
  FilePath ->
  IO [FlowView]
readViewsSQL UserProfile {..} sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select timestamp, version, flow_type, flow_data from event_log where flow_type != '__TRACE__' and flow_type != 'Note' order by timestamp"
    flows <- query_ cnx q
    pure $ reverse $ foldl (flip $ flowViewBuilder userName userTimezone userEndOfDay) [] flows

readCommandsSQL ::
  HasCallStack =>
  UserProfile ->
  FilePath ->
  IO [CommandView]
readCommandsSQL UserProfile {..} sqliteFile =
  withConnection sqliteFile $ \cnx -> do
    let q = "select timestamp, version, flow_type, flow_data from event_log where flow_Type = '__TRACE__' order by timestamp"
    traces <- query_ cnx q
    pure $ foldr (commandViewBuilder userTimezone) [] traces
