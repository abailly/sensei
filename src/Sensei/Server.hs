{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Sensei.Server where

import Control.Concurrent.MVar
import Control.Monad.Trans
import qualified Data.List as List
import Data.Text (Text)
import Sensei.Time hiding (getCurrentTime)
import Sensei.API
import Sensei.DB
import Servant
import Sensei.Version (senseiVersion, Versions(..))

killS ::
  MonadIO m => MVar () -> m ()
killS signal = liftIO (putMVar signal ())

setCurrentTimeS ::
  DB m => Text -> Timestamp -> m ()
setCurrentTimeS usr Timestamp{timestamp} = do
  usrProfile <- getUserProfileS usr
  setCurrentTime usrProfile timestamp

getCurrentTimeS ::
  DB m => Text -> m Timestamp
getCurrentTimeS usr = do
  usrProfile <- getUserProfileS usr
  Timestamp <$> getCurrentTime usrProfile

traceS ::
  (DB m) => Trace -> m ()
traceS trace =
  writeTrace trace

flowS ::
  (DB m) => Text -> FlowType -> FlowState -> m ()
flowS _ flowTyp flow =
  writeFlow (Flow flowTyp flow currentVersion)

updateFlowStartTimeS ::
  (DB m) => Text -> TimeDifference -> m FlowState
updateFlowStartTimeS _ timediff =
  updateLatestFlow (toNominalDiffTime timediff)

notesDayS ::
  (DB m) => Text -> Day -> m [NoteView]
notesDayS  usr day = do
  usrProfile <- getUserProfileS usr
  notes <- readNotes usrProfile
  pure $ map (uncurry NoteView) $ filter (sameDayThan day (localDay . fst)) notes

commandsDayS ::
  (DB m) => Text -> Day -> m [CommandView]
commandsDayS usr day = do
  usrProfile <- getUserProfileS usr
  commands <- readCommands usrProfile
  pure $ filter (commandOnDay day) commands

queryFlowDayS ::
  (DB m) => Text -> Day -> m [FlowView]
queryFlowDayS usr day = do
  usrProfile <- getUserProfileS usr
  views <- readViews usrProfile
  pure $ filter (flowOnDay day) views

queryFlowDaySummaryS ::
  (DB m) => Text -> Day -> m FlowSummary
queryFlowDaySummaryS usr day = do
  usrProfile <- getUserProfileS usr
  views <- readViews usrProfile
  commands <- readCommands usrProfile
  let summaryFlows = views
        |> filter (flowOnDay day)
        |> summarize
      summaryCommands = commands
        |> filter (commandOnDay day)
        |> summarize
      summaryPeriod = (day, day)
  pure $ FlowSummary{..}

queryFlowSummaryS ::
  (DB m) => Text -> m [GroupViews (FlowType, NominalDiffTime)]
queryFlowSummaryS usr = do
  usrProfile@UserProfile {userStartOfDay, userEndOfDay} <- getUserProfileS usr
  views <- groupViews userStartOfDay userEndOfDay [Day] <$> readViews usrProfile
  pure $ views |> fmap summary
  where
    summary :: GroupViews FlowView -> GroupViews (FlowType, NominalDiffTime)
    summary grp =
      ( case grp of
          NoViews -> NoViews
          (Leaf []) -> Leaf []
          (Leaf vs) -> Leaf (summarize vs)
          (GroupLevel g u gf) -> GroupLevel g u (summary gf)
      )

getFlowS ::
  (DB m) => Text -> Reference -> m (Maybe FlowView)
getFlowS _usr _ref = undefined

queryFlowS ::
  (DB m) => Text -> [Group] -> m [GroupViews FlowView]
queryFlowS usr groups = do
  usrProfile@UserProfile {userStartOfDay, userEndOfDay} <- getUserProfileS usr
  groupViews userStartOfDay userEndOfDay (List.sort groups) <$> readViews usrProfile

getLogS ::
  DB m => Text -> m [Event]
getLogS userName = do
  usrProfile <- getUserProfileS userName
  take 50 <$> readEvents usrProfile

getUserProfileS ::
  (DB m) => Text -> m UserProfile
getUserProfileS _ = do
  prof <- readProfile
  case prof of
    Left _ -> pure defaultProfile
    Right prf -> pure prf

putUserProfileS ::
  (DB m) => Text -> UserProfile -> m NoContent
putUserProfileS  _ profile = do
  writeProfile profile
  pure NoContent

getVersionsS ::
  (Monad m) => m Versions
getVersionsS = pure $ Versions senseiVersion senseiVersion currentVersion currentVersion
