{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.Server where

import Control.Concurrent.MVar
import Control.Monad.Trans
import qualified Data.List as List
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Network.HTTP.Link as Link
import Network.URI.Extra ()
import Sensei.API
import Sensei.DB
import Sensei.Time hiding (getCurrentTime)
import Sensei.Version (Versions (..), senseiVersion)
import Servant

killS ::
  MonadIO m => MVar () -> m ()
killS signal = liftIO (putMVar signal ())

setCurrentTimeS ::
  DB m => Text -> Timestamp -> m ()
setCurrentTimeS usr Timestamp {timestamp} = do
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
notesDayS usr day = do
  usrProfile <- getUserProfileS usr
  notes <- readNotes usrProfile (rangeFromDay day (userTimezone usrProfile))
  pure $ map (uncurry NoteView) notes

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
  let summaryFlows =
        views
          |> filter (flowOnDay day)
          |> summarize
      summaryCommands =
        commands
          |> filter (commandOnDay day)
          |> summarize
      summaryPeriod = (day, day)
  pure $ FlowSummary {..}

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
  (DB m) => Text -> Reference -> m (Maybe Flow)
getFlowS usr ref = do
  profile <- getUserProfileS usr
  readFlow profile ref

queryFlowS ::
  (DB m) => Text -> [Group] -> m [GroupViews FlowView]
queryFlowS usr groups = do
  usrProfile@UserProfile {userStartOfDay, userEndOfDay} <- getUserProfileS usr
  groupViews userStartOfDay userEndOfDay (List.sort groups) <$> readViews usrProfile

getLogS ::
  DB m => Text -> Maybe Natural -> m (Headers '[Header "Link" Text] [Event])
getLogS userName page = do
  usrProfile <- getUserProfileS userName
  EventsQueryResult {..} <- readEvents usrProfile (Page (fromMaybe 1 page) 50)
  let nextHeader =
        if endIndex < totalEvents
          then nextPageLink userName page
          else Nothing
      previousHeader =
        if startIndex > 1
          then previousPageLink userName page
          else Nothing
      links =
        case catMaybes [nextHeader, previousHeader] of
          [] -> noHeader
          ls -> addHeader $ writeLinkHeader ls
  pure $ links events

getUserProfileS ::
  (DB m) => Text -> m UserProfile
getUserProfileS _ = do
  prof <- readProfile
  case prof of
    Left _ -> pure defaultProfile
    Right prf -> pure prf

putUserProfileS ::
  (DB m) => Text -> UserProfile -> m NoContent
putUserProfileS _ profile = do
  writeProfile profile
  pure NoContent

getVersionsS ::
  (Monad m) => m Versions
getVersionsS = pure $ Versions senseiVersion senseiVersion currentVersion currentVersion
