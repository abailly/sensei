{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.Server where

import qualified Control.Exception.Safe as Exc
import Control.Monad.Trans
import Data.Aeson hiding (Options)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Time
import Sensei.API
import Servant
import System.IO

traceS :: FilePath -> Trace -> Handler ()
traceS file trace = liftIO $
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ encode trace <> "\n"
    hFlush out

flowS :: FilePath -> String -> FlowType -> FlowState -> Handler ()
flowS file _ flowTyp flow = liftIO $
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ encode (Flow flowTyp flow) <> "\n"
    hFlush out

-- | Read all the views for a given `UserProfile`
readViews :: FilePath -> UserProfile -> IO [FlowView]
readViews file (UserProfile usr tz _ dayEnd) =
  withBinaryFile file ReadMode $ loop (appendFlow tz dayEnd) usr []

loop :: (Flow -> [a] -> [a]) -> String -> [a] -> Handle -> IO [a]
loop f usr acc hdl = do
  res <- Exc.try $ LT.hGetLine hdl
  case res of
    Left (_ex :: Exc.IOException) -> pure (reverse acc)
    Right ln ->
      case eitherDecode (LT.encodeUtf8 ln) of
        Left _err -> loop f usr acc hdl
        Right flow -> loop f usr (flowView flow usr f acc) hdl

readNotes :: FilePath -> UserProfile -> IO [(LocalTime, Text)]
readNotes file UserProfile{userName,userTimezone} = withBinaryFile file ReadMode $ loop f userName []
  where
    f :: Flow -> [(LocalTime, Text)] -> [(LocalTime, Text)]
    f (Flow Note (FlowNote _ st _ note)) fragments =
      (utcToLocalTime userTimezone st, note) : fragments
    f _ fragments = fragments

notesDayS :: FilePath -> [Char] -> Day -> Handler [(LocalTime, Text)]
notesDayS file usr day = do
  usrProfile <- userProfileS usr
  notes <- liftIO $ readNotes file usrProfile
  pure $ filter (sameDayThan day (localDay . fst)) notes

queryFlowDayS :: FilePath -> [Char] -> Day -> Handler [FlowView]
queryFlowDayS file usr day = do
  usrProfile <- userProfileS usr
  views <- liftIO $ readViews file usrProfile
  pure $ filter (sameDayThan day (localDay . flowStart)) views

queryFlowDaySummaryS :: FilePath -> [Char] -> Day -> Handler [(FlowType, NominalDiffTime)]
queryFlowDaySummaryS file usr day = do
  usrProfile <- userProfileS usr
  views <- liftIO $ readViews file usrProfile
  pure $
    views
      |> filter (sameDayThan day (localDay . flowStart))
      |> summarize
  where

queryFlowSummaryS :: FilePath -> [Char] -> Handler [GroupViews (FlowType, NominalDiffTime)]
queryFlowSummaryS file usr = do
  usrProfile@UserProfile{userStartOfDay,userEndOfDay} <- userProfileS usr
  views <- liftIO $ groupViews userStartOfDay userEndOfDay [Day] <$> readViews file usrProfile
  pure $ views |> fmap summary
  where
    summary :: GroupViews FlowView -> GroupViews (FlowType, NominalDiffTime)
    summary grp = (case grp of
                       NoViews -> NoViews
                       (Leaf []) -> Leaf []
                       (Leaf vs) -> Leaf (summarize vs)
                       (GroupLevel g u gf) -> GroupLevel g u (summary gf))

-- summarize flows@(f NE.:| _) = (flowType f, sum $ fmap duration flows)

queryFlowS :: FilePath -> String -> [Group] -> Handler [GroupViews FlowView]
queryFlowS file usr groups = do
  usrProfile@UserProfile{userStartOfDay,userEndOfDay} <- userProfileS usr
  liftIO $ groupViews userStartOfDay userEndOfDay (List.sort groups) <$> readViews file usrProfile

flowView :: Flow -> String -> (Flow -> [a] -> [a]) -> [a] -> [a]
flowView f@Flow {..} usr mkView views =
  if _flowUser _flowState == usr
    then mkView f views
    else views

userProfileS :: String -> Handler UserProfile
userProfileS usr = pure UserProfile { userName = usr, userTimezone = hoursToTimeZone 1, userStartOfDay = TimeOfDay 08 00 00 , userEndOfDay = TimeOfDay 18 30 00  }
