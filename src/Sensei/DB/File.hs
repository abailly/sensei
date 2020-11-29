{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A "database" stored as a simple flat-file containing one line of JSON-formatted data per record.
module Sensei.DB.File
  ( FileDB(..), runFileDB,
    senseiLog, getDataFile
  )
where

import Control.Monad.Reader
import qualified Control.Exception.Safe as Exc
import Data.Aeson hiding (Options)
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack)
import Data.Time
import Sensei.DB
import Sensei.API
import System.Directory
import System.FilePath ((</>))
import System.IO

data FileDBPaths = FileDBPaths { storageFile :: FilePath,
                                 configDir :: FilePath
                               }

newtype FileDB a = FileDB { unFileDB :: ReaderT FileDBPaths IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runFileDB :: FilePath -> FilePath -> FileDB a -> IO a
runFileDB storage config =
  (`runReaderT` (FileDBPaths storage config)) . unFileDB

instance DB FileDB where
  initLogStorage = FileDB $ (asks storageFile >>= liftIO . initLogStorageFile)
  writeTrace t = FileDB $ (asks storageFile >>= liftIO . writeTraceFile t)
  writeFlow t = FileDB $ (asks storageFile >>= liftIO . writeFlowFile t)
  writeProfile u = FileDB $ (asks configDir >>= liftIO . writeProfileFile u)
  readViews u = FileDB $ (asks storageFile >>= liftIO . readViewsFile u)
  readNotes u = FileDB $ (asks storageFile >>= liftIO . readNotesFile u)
  readCommands u = FileDB $ (asks storageFile >>= liftIO . readCommandsFile u)
  readProfile = FileDB $ (asks configDir >>= liftIO . readProfileFile)

-- | Initialise a log store at given path
initLogStorageFile ::
  FilePath -> IO ()
initLogStorageFile output = do
  hasFile <- doesFileExist output
  unless hasFile $ openFile output WriteMode >>= hClose

writeTraceFile :: Trace -> FilePath -> IO ()
writeTraceFile trace file =
  writeJSON file (encode trace)

writeFlowFile :: Flow -> FilePath -> IO ()
writeFlowFile flow file =
  writeJSON file (encode flow)

writeJSON :: FilePath -> LBS.ByteString -> IO ()
writeJSON file jsonData =
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ jsonData <> "\n"
    hFlush out

-- | Read all the views for a given `UserProfile`
readViewsFile :: UserProfile -> FilePath -> IO [FlowView]
readViewsFile UserProfile {userName, userTimezone, userEndOfDay} file =
  withBinaryFile file ReadMode $ loop accumulator userName []
  where
    accumulator flow = flowView flow userName (appendFlow userTimezone userEndOfDay)

loop :: FromJSON b => (b -> [a] -> [a]) -> Text -> [a] -> Handle -> IO [a]
loop g usr acc hdl = do
  res <- Exc.try $ BS.hGetLine hdl
  case res of
    Left (_ex :: Exc.IOException) -> pure (reverse acc)
    Right ln ->
      case eitherDecode (LBS.fromStrict ln) of
        Left _err -> loop g usr acc hdl
        Right b -> loop g usr (g b acc) hdl

readNotesFile :: UserProfile -> FilePath -> IO [(LocalTime, Text)]
readNotesFile UserProfile {userName, userTimezone} file =
  withBinaryFile file ReadMode $ loop accumulator userName []
  where
    f :: Flow -> [(LocalTime, Text)] -> [(LocalTime, Text)]
    f (Flow Note (FlowNote _ st _ note) _) fragments =
      (utcToLocalTime userTimezone st, note) : fragments
    f _ fragments = fragments

    accumulator flow = flowView flow userName f

flowView :: Flow -> Text -> (Flow -> [a] -> [a]) -> [a] -> [a]
flowView f@Flow {..} usr mkView views =
  if _flowUser _flowState == usr
    then mkView f views
    else views

-- | Read all the views for a given `UserProfile`
readCommandsFile :: UserProfile -> FilePath -> IO [CommandView]
readCommandsFile UserProfile {userName, userTimezone} file =
  withBinaryFile file ReadMode $ loop readTrace userName []
  where
    readTrace t acc = mkCommandView userTimezone t : acc

-- | Read user profile file from given directory
-- The `UserProfile` is expected to be stored as a JSON-encoded file named `config.json`
-- in the given directory, which usually is the XDG configuration direcotry
-- for @sensei@, see <https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Specification> for more details.
-- Note the /current user/ is the owner of the process executing this function which
-- should be the same as the one running @ep@ command line.
readProfileFile ::
  FilePath -> IO (Either Text UserProfile)
readProfileFile home = do
  let configFile = home </> "config.json"
  existF <- doesFileExist configFile
  if (not existF)
    then pure $ Left (pack $ "config file " <> configFile <> " does not exist")
    else first pack . eitherDecode <$> LBS.readFile configFile

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
  newLog <- getDataDirectory >>= pure . (</> "sensei.log")
  maybeMigrateOldLog newLog
  pure newLog
  where
    maybeMigrateOldLog newLog = do
      oldLog <- senseiLog
      oldLogExists <- doesFileExist oldLog
      newLogExists <- doesFileExist newLog
      when (oldLogExists && not newLogExists) $ renameFile oldLog newLog

    getDataDirectory = do
      home <- getXdgDirectory XdgData "sensei"
      exists <- doesDirectoryExist home
      when (not exists) $ createDirectory home
      pure home
