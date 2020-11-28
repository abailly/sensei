{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.IO
  ( initLogStorage,
    writeTrace, writeFlow,
    readNotes,
    readViews,
    readCommands,
    readProfile,
    writeProfile,
  )
where

import qualified Control.Exception.Safe as Exc
import Control.Monad (unless)
import Data.Aeson hiding (Options)
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack)
import Data.Time
import Sensei.API
import System.Directory
import System.FilePath ((</>))
import System.IO

-- | Initialise a log store at given path
initLogStorage ::
  FilePath -> IO ()
initLogStorage output = do
  hasFile <- doesFileExist output
  unless hasFile $ openFile output WriteMode >>= hClose

writeTrace :: FilePath -> Trace -> IO ()
writeTrace file trace =
  writeJSON file (encode trace)

writeFlow :: FilePath -> Flow -> IO ()
writeFlow file flow =
  writeJSON file (encode flow)

writeJSON :: FilePath -> LBS.ByteString -> IO ()
writeJSON file jsonData =
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ jsonData <> "\n"
    hFlush out

-- | Read all the views for a given `UserProfile`
readViews :: FilePath -> UserProfile -> IO [FlowView]
readViews file UserProfile {userName, userTimezone, userEndOfDay} =
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

readNotes :: FilePath -> UserProfile -> IO [(LocalTime, Text)]
readNotes file UserProfile {userName, userTimezone} =
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
readCommands :: FilePath -> UserProfile -> IO [CommandView]
readCommands file UserProfile {userName, userTimezone} =
  withBinaryFile file ReadMode $ loop readTrace userName []
  where
    readTrace t acc = mkCommandView userTimezone t : acc

-- | Read user profile file from given directory
-- The `UserProfile` is expected to be stored as a JSON-encoded file named `config.json`
-- in the given directory, which usually is the XDG configuration direcotry
-- for @sensei@, see <https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Specification> for more details.
-- Note the /current user/ is the owner of the process executing this function which
-- should be the same as the one running @ep@ command line.
readProfile ::
  FilePath -> IO (Either Text UserProfile)
readProfile home = do
  let configFile = home </> "config.json"
  existF <- doesFileExist configFile
  if (not existF)
    then pure $ Left (pack $ "config file " <> configFile <> " does not exist")
    else first pack . eitherDecode <$> LBS.readFile configFile

writeProfile ::
  FilePath -> UserProfile -> IO ()
writeProfile home profile = do
  let configFile = home </> "config.json"
  LBS.writeFile configFile (encode profile)
