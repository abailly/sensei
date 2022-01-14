{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | A "database" stored as a simple flat-file containing one line of JSON-formatted data per record.
module Sensei.DB.File
  ( FileDB (..),
    runDB,
    getDataFile,
    readProfileFile,
    writeProfileFile,

    -- * Utility for migration
    readAll,
  )
where

import qualified Control.Exception.Safe as Exc
import Control.Monad.Reader
import Data.Aeson hiding (Options)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import Data.Text (Text)
import Sensei.API
import Sensei.DB
import Sensei.IO
import System.Directory
import System.FilePath ((</>))
import System.IO

data FileDBPaths = FileDBPaths
  { storageFile :: FilePath,
    configDir :: FilePath
  }

{-# DEPRECATED FileDB "This backend is deprecated in favor of Sensei.DB.SQLite" #-}

newtype FileDB a = FileDB {unFileDB :: ReaderT FileDBPaths IO a}
  deriving (Functor, Applicative, Monad, Exc.MonadThrow, Exc.MonadCatch, MonadIO)

runDB :: FilePath -> FilePath -> FileDB a -> IO a
runDB storage config =
  (`runReaderT` (FileDBPaths storage config)) . unFileDB

instance DB FileDB where
  type DBError FileDB = Exc.IOException

  initLogStorage = FileDB $ (asks storageFile >>= liftIO . initLogStorageFile)
  getCurrentTime = undefined
  setCurrentTime = undefined
  writeEvent t = FileDB $ (asks storageFile >>= liftIO . writeEventFile t)
  updateLatestFlow = undefined
  writeProfile u = FileDB $ (asks configDir >>= liftIO . writeProfileFile u)
  readEvents _ _ = do
    events <- readAll
    pure $ EventsQueryResult events 0 0 0 0
  readFlow _ _ = pure Nothing
  readViews u = FileDB $ (asks storageFile >>= liftIO . readViewsFile u)
  readNotes u _ = FileDB $ (asks storageFile >>= liftIO . readNotesFile u)
  readGoals _ = undefined
  searchNotes = undefined
  readCommands u = FileDB $ (asks storageFile >>= liftIO . readCommandsFile u)
  readProfile _ = FileDB $ (asks configDir >>= liftIO . readProfileFile)
  insertProfile _ = undefined

-- | Initialise a log store at given path
initLogStorageFile ::
  FilePath -> IO ()
initLogStorageFile output = do
  hasFile <- doesFileExist output
  unless hasFile $ openFile output WriteMode >>= hClose

readAll :: FileDB [EventView]
readAll = FileDB $ do
  file <- asks storageFile
  liftIO $ withBinaryFile file ReadMode $ \hdl -> reverse . snd <$> loop appendEventView "" (0, []) hdl
  where
    appendEventView e (index, acc) = (index + 1, EventView index e : acc)

writeEventFile :: Event -> FilePath -> IO ()
writeEventFile event file =
  writeJSON file (encode event)

writeJSON :: FilePath -> LBS.ByteString -> IO ()
writeJSON file jsonData =
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ jsonData <> "\n"
    hFlush out

-- | Read all the views for a given `UserProfile`
readViewsFile :: UserProfile -> FilePath -> IO [FlowView]
readViewsFile UserProfile {userName, userTimezone, userEndOfDay, userProjects} file =
  withBinaryFile file ReadMode $ \hdl -> reverse <$> loop (flowViewBuilder userName userTimezone userEndOfDay userProjects) userName [] hdl

loop :: FromJSON b => (b -> a -> a) -> Text -> a -> Handle -> IO a
loop g usr acc hdl = do
  res <- Exc.try $ BS.hGetLine hdl
  case res of
    Left (_ex :: Exc.IOException) -> pure acc
    Right ln ->
      case eitherDecode (LBS.fromStrict ln) of
        Left _err -> loop g usr acc hdl
        Right b -> loop g usr (g b acc) hdl

readNotesFile :: UserProfile -> FilePath -> IO [NoteView]
readNotesFile UserProfile {userName, userTimezone, userProjects} file =
  withBinaryFile file ReadMode $ \hdl -> reverse <$> loop (notesViewBuilder userName userTimezone userProjects) userName [] hdl

-- | Read all the views for a given `UserProfile`
readCommandsFile :: UserProfile -> FilePath -> IO [CommandView]
readCommandsFile UserProfile {userName, userTimezone, userProjects} file =
  withBinaryFile file ReadMode $ \hdl -> reverse <$> loop (commandViewBuilder userTimezone userProjects) userName [] hdl

-- | Read user profile file from given directory
-- The `UserProfile` is expected to be stored as a JSON-encoded file named `config.json`
-- in the given directory, which usually is the XDG configuration direcotry
-- for @sensei@, see <https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Specification> for more details.
-- Note the /current user/ is the owner of the process executing this function which
-- should be the same as the one running @ep@ command line.
readProfileFile ::
  FilePath -> IO UserProfile
readProfileFile home = do
  let configFile = home </> "config.json"
  content <- eitherDecode <$> LBS.readFile configFile
  case content of
    Left e -> Exc.throwIO $ userError ("invalid configuration file '" <> configFile <> "': " <> show e)
    Right p -> pure p

writeProfileFile ::
  UserProfile -> FilePath -> IO ()
writeProfileFile profile home = do
  let configFile = home </> "config.json"
  LBS.writeFile configFile (encode profile)

{-# DEPRECATED senseiLog "this will be removed in favor of XDG data directory storage" #-}
senseiLog :: IO FilePath
senseiLog = (</> ".sensei.log") <$> getHomeDirectory

getDataFile :: IO FilePath
getDataFile = do
  newLog <- getDataDirectory <&> (</> "sensei.log")
  maybeMigrateOldLog newLog
  pure newLog
  where
    maybeMigrateOldLog newLog = do
      oldLog <- senseiLog
      oldLogExists <- doesFileExist oldLog
      newLogExists <- doesFileExist newLog
      when (oldLogExists && not newLogExists) $ renameFile oldLog newLog
