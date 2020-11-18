{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.Server where

import Control.Monad.Trans
import Data.Aeson hiding (Options)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.Text (Text)
import Data.Time
import Sensei.API
import Servant
import System.IO
import Sensei.IO
import Control.Concurrent.MVar

killS :: MVar () -> Handler ()
killS signal = liftIO (putMVar signal ())

traceS ::
  FilePath -> Trace -> Handler ()
traceS file trace = liftIO $
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ encode trace <> "\n"
    hFlush out

flowS ::
  FilePath -> Text -> FlowType -> FlowState -> Handler ()
flowS file _ flowTyp flow = liftIO $
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ encode (Flow flowTyp flow currentVersion) <> "\n"
    hFlush out


notesDayS ::
  FilePath -> Text -> Day -> Handler [(LocalTime, Text)]
notesDayS file usr day = do
  usrProfile <- userProfileS usr
  notes <- liftIO $ readNotes file usrProfile
  pure $ filter (sameDayThan day (localDay . fst)) notes

queryFlowDayS ::
  FilePath -> Text -> Day -> Handler [FlowView]
queryFlowDayS file usr day = do
  usrProfile <- userProfileS usr
  views <- liftIO $ readViews file usrProfile
  pure $ filter (sameDayThan day (localDay . flowStart)) views

queryFlowDaySummaryS ::
  FilePath -> Text -> Day -> Handler [(FlowType, NominalDiffTime)]
queryFlowDaySummaryS file usr day = do
  usrProfile <- userProfileS usr
  views <- liftIO $ readViews file usrProfile
  pure $
    views
      |> filter (sameDayThan day (localDay . flowStart))
      |> summarize
  where

queryFlowSummaryS ::
  FilePath -> Text -> Handler [GroupViews (FlowType, NominalDiffTime)]
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

queryFlowS ::
  FilePath -> Text -> [Group] -> Handler [GroupViews FlowView]
queryFlowS file usr groups = do
  usrProfile@UserProfile{userStartOfDay,userEndOfDay} <- userProfileS usr
  liftIO $ groupViews userStartOfDay userEndOfDay (List.sort groups) <$> readViews file usrProfile

userProfileS ::
  Text -> Handler UserProfile
userProfileS usr = pure UserProfile { userName = usr, userTimezone = hoursToTimeZone 1, userStartOfDay = TimeOfDay 08 00 00 , userEndOfDay = TimeOfDay 18 30 00  }
