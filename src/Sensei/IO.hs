{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.IO (readNotes, readViews, readCommands) where

import qualified Control.Exception.Safe as Exc
import Data.Aeson hiding (Options)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Time
import Sensei.API
import System.IO

-- | Read all the views for a given `UserProfile`
readViews :: FilePath -> UserProfile -> IO [FlowView]
readViews file (UserProfile usr tz _ dayEnd) =
  withBinaryFile file ReadMode $ loop accumulator usr []
  where
    accumulator flow = flowView flow usr (appendFlow tz dayEnd)

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
readCommands :: FilePath -> UserProfile -> IO [Trace]
readCommands file (UserProfile usr _ _ _) =
  withBinaryFile file ReadMode $ loop readTrace usr []
  where
    readTrace t acc = t : acc
