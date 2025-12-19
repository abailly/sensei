{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Implementation of flows DB using <https://www.sqlite.org/index.html SQLite>
-- embedded DB.
--
-- The DB is expected to have the following schema:
-- @@
-- CREATE TABLE users (
--    id integer primary key,
--    uid text unique not null,
--    user text unique not null,
--    profile text not null);
--
-- CREATE TABLE schema_migrations ( id integer primary key,
--                                  filename text not null,
--                                  checksum text not null,
--                                  executed_at integer default (strftime('%s','now')) not null);
--
-- CREATE TABLE event_log ( id integer primary key,
--                          timestamp text not null,
--                          version integer not null,
--                          user text not null,
--                          flow_type text not null,
--                          flow_data text not null);
--
-- CREATE TABLE config_log ( id integer primary key,
--                           timestamp text not null,
--                           version integer not null,
--                           user text not null,
--                           key text not null,
--                           value text not null);
-- @@
--
-- The actual data is stored in the `flow_data` field as JSON.
module Sensei.DB.SQLite
  ( runDB,
    getDataFile,
    migrateFileDB,
    SQLiteDB,
    SQLiteDBError (..),
    withBackup,
    readAll,
  )
where

import Control.Exception (throwIO)
import Control.Exception.Safe
  ( Exception,
    Handler (Handler),
    IOException,
    MonadCatch,
    MonadThrow,
    catch,
    catches,
    throwM,
    try,
  )
import Control.Lens ((^.))
import Control.Monad (forM_, unless)
import Control.Monad.Reader
  ( MonadIO (..),
    MonadReader (ask),
    ReaderT (..),
    asks,
  )
import Crypto.Hash
import Data.Aeson (eitherDecode, encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void, (<&>))
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import qualified Data.Time as Time
import Database.SQLite.Simple
  ( Connection,
    FormatError,
    FromRow (..),
    Only (Only),
    Query,
    ResultError,
    SQLData (SQLInteger, SQLText),
    SQLError,
    ToRow (..),
    field,
    withTransaction,
  )
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Preface.Codec
  ( Encoded,
    Hex,
    toHex,
    pattern EncodedHex,
  )
import Preface.Log (LoggerEnv (..), fakeLogger)
import Preface.Utils (decodeUtf8', toText)
import Sensei.API as API
  ( Article (..),
    CommandView,
    Event (..),
    EventView (..),
    Flow (..),
    FlowView,
    GoalOp (_goalUser),
    NoteFlow (..),
    NoteView,
    Reference (Latest, Pos),
    TimeRange (..),
    UserProfile (..),
    eventTimestamp,
    filterNotes,
    getGoal,
    parseEventFromv4,
    parseFlowFromv4,
    parseFlowType,
    parseNoteFromv4,
    user',
  )
import Sensei.DB
  ( DB (..),
    EventsQueryResult (..),
    Pagination (..),
    commandViewBuilder,
    flowViewBuilder,
    notesViewBuilder,
    toNoteView,
  )
import qualified Sensei.DB.File as File
import Sensei.DB.SQLite.Migration
  ( Migration (InitialMigration),
    MigrationResult (..),
    addUniqueConstraintToUsers,
    addUserInEventLog,
    createArticles,
    createConfig,
    createLog,
    createNotesSearch,
    createUsers,
    populateSearch,
    runMigrations,
    updateUserInEventLog,
  )
import Sensei.IO (getDataDirectory)
import Sensei.Version (currentVersion)
import System.Directory
  ( copyFile,
    doesFileExist,
    removeFile,
    renameFile,
  )
import System.FilePath ((<.>), (</>))
import System.IO (IOMode (WriteMode), hClose, openFile)
import System.Random (newStdGen, randoms)

-- | The configuration for DB engine.
data SQLiteConfig = SQLiteConfig
  { -- | Path to SQLite database file
    storagePath :: FilePath,
    -- | Directory where user-based configuration is stored. Note that
    --  we reuse the file-based configuration mechanism from `Sensei.DB.File`, it
    --  would probably be better to store everything into the SQLite DB file.
    configDir :: FilePath,
    -- | The `LoggerEnv` structure to use for logging this DB's actions
    logger :: LoggerEnv
  }

-- | Simple monad stack to run SQLite actions within the context of a `SQLiteConfig`.
newtype SQLiteDB a = SQLiteDB {unSQLite :: ReaderT SQLiteConfig IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader SQLiteConfig, MonadThrow, MonadCatch, MonadIO)

-- | Runs a DB action using given DB file and configuration directory.
runDB ::
  (HasCallStack) =>
  FilePath ->
  FilePath ->
  LoggerEnv ->
  SQLiteDB a ->
  IO a
runDB dbFile configDir logger act =
  unSQLite act `runReaderT` SQLiteConfig dbFile configDir logger

getDataFile :: FilePath -> IO FilePath
getDataFile configDir = do
  oldLog <- getDataDirectory <&> (</> "sensei.log")
  newLog <- getDataDirectory <&> (</> "sensei.sqlite")
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
        Right _ -> do
          renameFile old new
          pure new

-- | Migrate an existing File-based event log to a SQLite-based one.
--  The old database is preserved and copied to a new File with same name
--  but suffixed `.old`.
--  Returns either an error description or the (absolute) path to the old
--  file-based event log.
migrateFileDB ::
  (HasCallStack) =>
  FilePath ->
  FilePath ->
  IO (Either Text FilePath)
migrateFileDB dbFile configDir = do
  let oldLog = dbFile <.> "old"
  renameFile dbFile oldLog
  evs <- try $ File.runDB oldLog configDir File.readAll
  case evs of
    Left (e :: IOException) -> pure $ Left (pack $ show e)
    Right events -> runDB dbFile configDir fakeLogger $ do
      initSQLiteDB
      writeAll (fmap API.event events)
      pure $ Right oldLog

-- * Instances

instance ToRow Event where
  toRow e =
    let ts = toField (eventTimestamp e)
        u = e ^. user'
        payload = toField $ decodeUtf8' $ LBS.toStrict $ encode e
        typ = typeOf e
     in [ts, SQLInteger (fromIntegral currentVersion), toField u, toField (typ :: Text), payload]

instance FromRow Event where
  fromRow = do
    -- id, timestamp, version, flow_type, flow_data
    _id :: Int <- field
    _ts :: UTCTime <- field
    _ver :: Integer <- field
    _ty :: Text <- field
    either error id . eitherDecode . encodeUtf8 <$> field

typeOf :: Event -> Text
typeOf EventTrace {} = "__TRACE__"
typeOf (EventFlow Flow {_flowType}) = toText _flowType
typeOf EventNote {} = "Note"
typeOf EventGoal {} = "Goal"
typeOf EventArticle {} = "Article"

instance FromRow EventView where
  fromRow = do
    index <- fromInteger <$> field
    _ts :: UTCTime <- field
    ver :: Integer <- field
    ty :: Text <- field
    event <-
      if ver >= 5
        then either error id . eitherDecode . encodeUtf8 <$> field
        else case ty of
          "__TRACE__" -> decodeTracev4 <$> field
          "Note" -> decodeNotev4 <$> field
          _ -> decodeFlowv4 ty <$> field
    pure $ EventView {..}
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

-- | This instance returns a number to be used as offset value in a query
instance ToField Reference where
  toField Latest = SQLInteger 0
  toField (Pos n) = SQLInteger (fromIntegral n)

-- * DB Implementation

-- ** Error Handling

data SQLiteDBError = SQLiteDBError Query Text
  deriving (Eq, Show)

instance Exception SQLiteDBError

-- | Repack errors thrown by `SQLite` engine into a `SQLiteDBError`.
handleErrors :: LoggerEnv -> Query -> IO a -> IO a
handleErrors logger q m =
  m
    `catches` [ Handler $ \(e :: FormatError) -> handle e,
                Handler $ \(e :: ResultError) -> handle e,
                Handler $ \(e :: SQLError) -> handle e
              ]
  where
    handle :: (Show e) => e -> IO a
    handle e = do
      logError logger (ErrorRunningQuery q (pack $ show e))
      throwM $ SQLiteDBError q (pack $ show e)

execute :: (ToRow q) => Connection -> LoggerEnv -> Query -> q -> IO ()
execute cnx logger q args =
  withLog logger (ExecutingQuery q (Just $ pack $ show $ toRow args)) $
    handleErrors logger q $
      SQLite.execute cnx q args

query :: (ToRow q, FromRow r) => Connection -> LoggerEnv -> Query -> q -> IO [r]
query cnx logger q args =
  withLog logger (ExecutingQuery q (Just $ pack $ show $ toRow args)) $
    handleErrors logger q $
      SQLite.query cnx q args

query_ :: (FromRow r) => Connection -> LoggerEnv -> Query -> IO [r]
query_ cnx logger q =
  withLog logger (ExecutingQuery q Nothing) $
    handleErrors logger q $
      SQLite.query_ cnx q

withConnection :: (Connection -> IO a) -> SQLiteDB a
withConnection act =
  asks storagePath >>= liftIO . (`SQLite.withConnection` act)

instance DB SQLiteDB where
  type DBError SQLiteDB = SQLiteDBError
  initLogStorage = initSQLiteDB
  setCurrentTime u ts = setCurrentTimeSQL u ts
  getCurrentTime u = getCurrentTimeSQL u
  writeEvent t = writeEventSQL t
  updateLatestFlow ts = updateLatestFlowSQL ts
  readFlow u r = readFlowSQL u r
  readEvents u p = readEventsSQL u p
  readNotes u rge = readNotesSQL u rge
  readGoals u = readGoalsSQL u
  searchNotes u txt = searchNotesSQL u txt
  readViews u = readViewsSQL u
  readCommands u = readCommandsSQL u
  readProfile = readProfileSQL
  readProfileById = readProfileByIdSQL
  writeProfile = writeProfileSQL
  insertProfile = insertProfileSQL

data Events
  = StoragePathCreated {dbPath :: FilePath}
  | MigratingSQLiteDB {dbPath :: FilePath}
  | ExecutingQuery {queryRun :: Query, queryArgs :: Maybe Text}
  | ErrorRunningQuery {queryRun :: Query, errorText :: Text}
  deriving (Eq, Show, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)

-- ** Orphan Instances

-- initializes the DB file
-- if the file does not exist, it is created
initSQLiteDB :: SQLiteDB ()
initSQLiteDB = do
  SQLiteConfig {storagePath, logger, configDir} <- ask
  storagePathExists <- liftIO $ doesFileExist storagePath
  unless storagePathExists $
    liftIO $ do
      openFile storagePath WriteMode >>= hClose
      logInfo logger (StoragePathCreated storagePath)
  liftIO $
    withLog logger (MigratingSQLiteDB storagePath) $ do
      migrateSQLiteDB logger storagePath
      migrateFileBasedProfile logger storagePath configDir

migrateSQLiteDB :: LoggerEnv -> FilePath -> IO ()
migrateSQLiteDB logger sqliteFile =
  withBackup sqliteFile $ \file ->
    SQLite.withConnection file $ \cnx -> do
      migResult <-
        runMigrations
          logger
          cnx
          [ InitialMigration,
            createLog,
            createConfig,
            createNotesSearch,
            populateSearch,
            createUsers,
            addUserInEventLog,
            updateUserInEventLog,
            addUniqueConstraintToUsers,
            createArticles
          ]
      case migResult of
        MigrationFailed err -> throwIO $ SQLiteDBError "" err
        MigrationSuccessful -> pure ()
        MigrationAlreadyApplied -> pure ()

withBackup :: FilePath -> (FilePath -> IO a) -> IO a
withBackup file action = do
  timestamp <- Time.formatTime Time.defaultTimeLocale "%s" <$> Time.getCurrentTime
  let backup = file <.> "bak" <.> timestamp
  copyFile file backup
  (action file <* removeFile backup)
    `catch` \(e :: SQLiteDBError) -> copyFile backup file >> removeFile backup >> throwIO e

migrateFileBasedProfile :: LoggerEnv -> FilePath -> FilePath -> IO ()
migrateFileBasedProfile logger storagePath config = do
  eitherProfile <- try @_ @IOException $ File.readProfileFile config
  case eitherProfile of
    Left _ -> pure ()
    Right profile@UserProfile {userName} ->
      runDB storagePath config logger $ do
        void (readProfileSQL userName)
          `catch` \(_ :: SQLiteDBError) -> void (insertProfileSQL profile)

setCurrentTimeSQL :: UserProfile -> UTCTime -> SQLiteDB ()
setCurrentTimeSQL UserProfile {userName} newTime = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "insert into config_log(timestamp, version, user, key, value) values (?,?,?,?,?)"
    ts <- Time.getCurrentTime -- This is silly, isn't it?
    execute cnx logger q [toField ts, toField (fromIntegral currentVersion :: Integer), toField userName, toField ("currentTime" :: Text), toField newTime]

getCurrentTimeSQL :: UserProfile -> SQLiteDB UTCTime
getCurrentTimeSQL UserProfile {userName} = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select value from config_log where user = ? and key = 'currentTime' order by timestamp desc limit 1"
    res <- query cnx logger q (Only userName)
    case res of
      ([ts] : _) -> pure ts
      _ -> Time.getCurrentTime

writeAll ::
  (HasCallStack) =>
  [Event] ->
  SQLiteDB ()
writeAll events = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> forM_ events (insert cnx logger)

readAll ::
  (HasCallStack) =>
  SQLiteDB [Event]
readAll = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select id, timestamp, version, flow_type, flow_data from event_log order by timestamp asc"
    query_ cnx logger q

updateNotesIndex ::
  (HasCallStack) =>
  Int ->
  Event ->
  Connection ->
  LoggerEnv ->
  IO ()
updateNotesIndex rid (EventNote NoteFlow {..}) cnx logger =
  let q = "insert into notes_search (id, note) values (?, ?)"
   in execute cnx logger q [toField rid, SQLText _noteContent]
updateNotesIndex _ _ _ _ = pure ()

-- | Compute SHA256 hash of article content
computeArticleHash :: Text -> Encoded Hex
computeArticleHash content =
  toHex $ convert $ hashWith SHA256 $ Text.encodeUtf8 content

-- | Store article content in articles table
storeArticleContent ::
  Connection ->
  LoggerEnv ->
  Encoded Hex ->
  UTCTime ->
  Text ->
  IO ()
storeArticleContent cnx logger h timestamp content = do
  let q = "insert or ignore into articles (hash, timestamp, content) values (?, ?, ?)"
  execute cnx logger q (toText h, timestamp, Text.encodeUtf8 content)

-- | Retrieve article content by hash
retrieveArticleContent ::
  Connection ->
  LoggerEnv ->
  Text ->
  IO (Maybe Text)
retrieveArticleContent cnx logger hashText = do
  let q = "select content from articles where hash = ?"
  results <- query cnx logger q (Only hashText) :: IO [[BS.ByteString]]
  pure $ case results of
    [[content]] -> Just (Text.decodeUtf8 content)
    _ -> Nothing

-- | Restore article content from hash in an EventView
restoreArticleContent ::
  Connection ->
  LoggerEnv ->
  EventView ->
  IO EventView
restoreArticleContent cnx logger ev@EventView {event = EventArticle article} = do
  case article of
    PublishArticle {_article = hashText} | T.length hashText == 64 -> do
      maybeContent <- retrieveArticleContent cnx logger hashText
      case maybeContent of
        Just content -> pure $ ev {event = EventArticle article {_article = content}}
        Nothing -> pure ev
    UpdateArticle {_article = hashText} | T.length hashText == 64 -> do
      maybeContent <- retrieveArticleContent cnx logger hashText
      case maybeContent of
        Just content -> pure $ ev {event = EventArticle article {_article = content}}
        Nothing -> pure ev
    _ -> pure ev
restoreArticleContent _ _ ev = pure ev

writeEventSQL ::
  (HasCallStack) =>
  Event ->
  SQLiteDB ()
writeEventSQL e = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    -- Handle EventArticle specially: store content and replace with hash
    e' <- case e of
      EventArticle articleOp@(PublishArticle {_article = content, _articleTimestamp = timestamp})
        | not (T.null content) -> do
            let h = computeArticleHash content
            storeArticleContent cnx logger h timestamp content
            pure $ EventArticle articleOp {_article = toText h}
      EventArticle articleOp@(UpdateArticle {_article = content, _articleTimestamp = timestamp})
        | not (T.null content) -> do
            let h = computeArticleHash content
            storeArticleContent cnx logger h timestamp content
            pure $ EventArticle articleOp {_article = toText h}
      _ -> pure e
    rid <- insert cnx logger e'
    updateNotesIndex rid e' cnx logger

insert ::
  (HasCallStack) =>
  (ToRow q) =>
  Connection ->
  LoggerEnv ->
  q ->
  IO Int
insert cnx logger event = do
  let q = "insert into event_log (timestamp, version, user, flow_type, flow_data) values (?, ?, ?, ?, ?)"
  execute cnx logger q event
  [[r]] <- query_ cnx logger "SELECT last_insert_rowid()"
  pure r

updateLatestFlowSQL ::
  (HasCallStack) =>
  NominalDiffTime ->
  SQLiteDB Event
updateLatestFlowSQL diff = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx ->
    withTransaction cnx $ do
      let q = "select id, timestamp, version, flow_type, flow_data from event_log where flow_type != '__TRACE__' order by timestamp desc limit 1"
          u = "update event_log set timestamp = ?, flow_data = ? where id = ?"
      res <- query_ cnx logger q
      case res of
        (EventView {index, event = EventFlow f@Flow {_flowTimestamp}} : _) -> do
          let newTs = addUTCTime diff _flowTimestamp
              updatedFlow = EventFlow f {_flowTimestamp = newTs}
          execute cnx logger u [toField newTs, toField $ decodeUtf8' $ LBS.toStrict $ encode updatedFlow, toField (toInteger index)]
          pure updatedFlow
        (EventView {event} : _) -> pure event -- don't change other events
        [] -> error "no flows recorded"

readFlowSQL ::
  (HasCallStack) =>
  UserProfile ->
  Reference ->
  SQLiteDB (Maybe EventView)
readFlowSQL UserProfile {userName} ref = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select id, timestamp, version, flow_type, flow_data from event_log where flow_type != '__TRACE__' and user = ? order by timestamp desc limit 1 offset ?"
    res <- query cnx logger q (userName, ref)
    case res of
      [flow] -> Just <$> restoreArticleContent cnx logger flow
      [] -> pure Nothing
      _ -> error "invalid query results"

readEventsSQL ::
  (HasCallStack) =>
  UserProfile ->
  Pagination ->
  SQLiteDB EventsQueryResult
readEventsSQL UserProfile {userName} Page {pageNumber, pageSize} = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select id, timestamp, version, flow_type, flow_data from event_log where user = ? order by timestamp desc limit ? offset ?"
        count = "select count(*) from event_log where user = ?"
        limit = SQLInteger $ fromIntegral pageSize
        offset = SQLInteger $ fromIntegral $ (pageNumber - 1) * pageSize
    events <- query cnx logger q (userName, limit, offset)
    resultEvents <- mapM (restoreArticleContent cnx logger) events
    [[numEvents]] <- query cnx logger count (Only userName)
    let totalEvents = fromInteger numEvents
        eventsCount = fromIntegral $ length resultEvents
        startIndex = min ((pageNumber - 1) * pageSize) totalEvents
        endIndex = min (pageNumber * pageSize) totalEvents
    pure $ EventsQueryResult {..}
readEventsSQL UserProfile {userName} NoPagination = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select id, timestamp, version, flow_type, flow_data from event_log where user = ? order by timestamp asc"
        count = "select count(*) from event_log where user = ?"
    events <- query cnx logger q (Only userName)
    resultEvents <- mapM (restoreArticleContent cnx logger) events
    [[numEvents]] <- query cnx logger count (Only userName)
    let totalEvents = fromInteger numEvents
        eventsCount = totalEvents
        startIndex = 1
        endIndex = totalEvents
    pure $ EventsQueryResult {..}

readNotesSQL ::
  (HasCallStack) =>
  UserProfile ->
  TimeRange ->
  SQLiteDB [NoteView]
readNotesSQL UserProfile {..} TimeRange {..} = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select id, timestamp, version, flow_type, flow_data from event_log where flow_type = 'Note' and user = ? and datetime(timestamp) between ? and ? order by timestamp"
    notesFlow <- query cnx logger q (userName, rangeStart, rangeEnd)
    pure $ foldr (notesViewBuilder userName userTimezone userProjects) [] notesFlow

searchNotesSQL ::
  (HasCallStack) =>
  UserProfile ->
  Text ->
  SQLiteDB [NoteView]
searchNotesSQL UserProfile {..} text = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select event_log.id, timestamp, version, flow_type, flow_data from event_log inner join notes_search on event_log.id = notes_search.id where notes_search match ?"
    notesFlow <- query cnx logger q [text]
    pure $ fmap (toNoteView userTimezone userProjects) (filterNotes $ fmap API.event notesFlow)

readViewsSQL ::
  (HasCallStack) =>
  UserProfile ->
  SQLiteDB [FlowView]
readViewsSQL UserProfile {..} = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select id, timestamp, version, flow_type, flow_data from event_log where flow_type != '__TRACE__' and flow_type != 'Note' order by timestamp"
    flows <- query_ cnx logger q
    pure $ reverse $ foldl (flip $ flowViewBuilder userName userTimezone userEndOfDay userProjects) [] flows

readGoalsSQL ::
  (HasCallStack) =>
  UserProfile ->
  SQLiteDB [GoalOp]
readGoalsSQL UserProfile {userName} = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select id, timestamp, version, flow_type, flow_data from event_log where flow_type = 'Goal' order by timestamp"
    flows <- query_ cnx logger q
    pure $ filter ((== userName) . _goalUser) $ mapMaybe (getGoal . event) flows

readCommandsSQL ::
  (HasCallStack) =>
  UserProfile ->
  SQLiteDB [CommandView]
readCommandsSQL UserProfile {..} = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select id, timestamp, version, flow_type, flow_data from event_log where flow_Type = '__TRACE__' and user = ? order by timestamp"
    traces <- query cnx logger q (Only userName)
    pure $ foldr (commandViewBuilder userTimezone userProjects) [] traces

readProfileSQL ::
  (HasCallStack) =>
  Text ->
  SQLiteDB UserProfile
readProfileSQL userName = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select profile, uid from users where user = ?"
    res <- query cnx logger q (Only userName)
    case res of
      [[profile, uid]] -> case eitherDecode . encodeUtf8 $ profile of
        Left e -> throwM $ SQLiteDBError q (pack $ "Fail to decode user profile: " <> e)
        Right p -> pure p {userId = EncodedHex (toStrict uid)}
      [] -> throwM $ SQLiteDBError q ("No user " <> userName)
      _ -> throwM $ SQLiteDBError q ("Several users with " <> userName <> ", this is a bug")

readProfileByIdSQL ::
  (HasCallStack) =>
  Encoded Hex ->
  SQLiteDB UserProfile
readProfileByIdSQL uid = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "select profile from users where uid = ?"
    res <- query cnx logger q (Only $ toText uid)
    case res of
      [[profile]] -> case eitherDecode . encodeUtf8 $ profile of
        Left e -> throwM $ SQLiteDBError q (pack $ "Fail to decode user profile: " <> e)
        Right p -> pure p
      [] -> throwM $ SQLiteDBError q ("No user with id " <> toText uid)
      _ -> throwM $ SQLiteDBError q ("Several users with id " <> toText uid <> ", this is a bug")

writeProfileSQL ::
  (HasCallStack) =>
  UserProfile ->
  SQLiteDB ()
writeProfileSQL profile = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "update users set profile = ? where user = ?;"
    execute cnx logger q [toField $ decodeUtf8' $ LBS.toStrict $ encode profile, toField (userName profile)]

insertProfileSQL ::
  (HasCallStack) =>
  UserProfile ->
  SQLiteDB (Encoded Hex)
insertProfileSQL profile = do
  SQLiteConfig {logger} <- ask
  withConnection $ \cnx -> do
    let q = "insert into users (uid, user, profile) values (?,?,?);"
    userId <- toHex . BS.pack . take 16 . randoms <$> newStdGen
    let profileWithId = profile {userId}
    execute cnx logger q [toField $ toText userId, toField (userName profile), toField $ decodeUtf8' $ LBS.toStrict $ encode profileWithId]
    pure userId
