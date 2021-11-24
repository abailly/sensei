{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.FlowView where

import Data.Aeson hiding (Options)
import Data.Function (on)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Text.ToText (ToText (..))
import Data.Time
import GHC.Generics
import Sensei.Event (Event (..))
import Sensei.Flow
import Sensei.Project
import Sensei.Summary
import Sensei.Time
import Sensei.Utils

-- | A view on a single event
data EventView = EventView {index :: Natural, event :: Event}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | A single note
data NoteView = NoteView
  { noteStart :: LocalTime,
    noteView :: Text,
    noteProject :: ProjectName,
    noteTags :: [Tag]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Tag = Tag Text
  deriving newtype (Eq, Show, ToJSON, FromJSON)

instance ToText Tag where
  toText (Tag t) = t

-- | A view of an executed command
data CommandView = CommandView
  { commandStart :: LocalTime,
    commandProcess :: Text,
    commandElapsed :: NominalDiffTime,
    commandProject :: ProjectName
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance HasSummary CommandView Text where
  summarize commands =
    commands
      |> List.sortBy (compare `on` commandProcess)
      |> NE.groupBy ((==) `on` commandProcess)
      |> fmap (\cmds@(f :| _) -> (commandProcess f, sum $ fmap commandElapsed cmds))

mkCommandView ::
  TimeZone -> ProjectsMap -> Event -> Maybe CommandView
mkCommandView tz projectsMap (EventTrace Trace {..}) =
  Just
    CommandView
      { commandStart = utcToLocalTime tz _traceTimestamp,
        commandProcess = _traceProcess,
        commandElapsed = _traceElapsed,
        commandProject = projectsMap `selectProject` _traceDirectory
      }
mkCommandView _ _ _ = Nothing

commandInPeriod ::
  Maybe LocalTime -> Maybe LocalTime -> CommandView -> Bool
commandInPeriod (Just lb) (Just ub) = withinPeriod lb ub commandStart
commandInPeriod _ _ = undefined

-- | A single "flow" timeslice of a given type
-- `FlowView`s times are expressed in the `LocalTime` of the users to which they
-- pertain.
data FlowView = FlowView
  { flowStart :: LocalTime,
    flowEnd :: LocalTime,
    viewType :: FlowType,
    flowProject :: ProjectName
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

flowInPeriod ::
  Maybe LocalTime -> Maybe LocalTime -> FlowView -> Bool
flowInPeriod (Just lb) (Just ub) = withinPeriod lb ub flowStart
flowInPeriod _ _ = undefined

projectInPeriod ::
  Maybe LocalTime -> Maybe LocalTime -> FlowView -> Bool
projectInPeriod (Just lb) (Just ub) = withinPeriod lb ub flowStart
projectInPeriod _ _ = undefined

appendFlow :: TimeZone -> TimeOfDay -> ProjectsMap -> EventView -> [FlowView] -> [FlowView]
appendFlow _ _ _ (EventView {event = EventFlow (Flow {_flowType = End})}) [] = []
appendFlow tz _ _ (EventView {event = EventFlow (Flow {_flowType = End, ..})}) (v : vs) =
  v {flowEnd = utcToLocalTime tz _flowTimestamp} : vs
appendFlow tz dayEnd projectsMap (EventView {event = EventFlow (Flow {..})}) views =
  let view = FlowView st st _flowType (projectsMap `selectProject` _flowDir)
      st = utcToLocalTime tz _flowTimestamp
   in case views of
        (v : vs) -> view : fillFlowEnd dayEnd v st : vs
        [] -> [view]
appendFlow _ _ _ _ views = views

-- OUCH
fillFlowEnd :: TimeOfDay -> FlowView -> LocalTime -> FlowView
fillFlowEnd endOfDay v st
  | flowStart v == flowEnd v =
    if localDay st == localDay (flowStart v)
      then v {flowEnd = st}
      else
        let end = LocalTime (localDay (flowStart v)) endOfDay
            oneHour = secondsToNominalDiffTime 3600
            plus1hour = addLocalTime oneHour (flowStart v)
         in if end < flowStart v
              then v {flowEnd = plus1hour}
              else v {flowEnd = end}
  | otherwise = v

-- | Normalize the given list of views to ensure it starts and ends at "standard" time.
-- This function is useful to provide a common scale when comparing flows across several days
-- which might not start nor end at the same time.
-- This function assumes that:
-- 1. The given `FlowView` list is sorted
-- 2. All `FlowView` are for the same day
normalizeViewsForDay :: LocalTime -> LocalTime -> [FlowView] -> [FlowView]
normalizeViewsForDay startOfDay endOfDay [] =
  [FlowView startOfDay endOfDay Other ""]
normalizeViewsForDay startOfDay endOfDay views@(FlowView {flowStart, flowProject} : _) =
  let st' =
        if flowStart > startOfDay
          then [FlowView {flowStart = startOfDay, flowEnd = flowStart, viewType = Other, flowProject}]
          else []
      end' = normalizeLastView endOfDay views
   in st' <> end'

normalizeLastView :: LocalTime -> [FlowView] -> [FlowView]
normalizeLastView _ [] = []
normalizeLastView endOfDay [end@FlowView {flowStart, flowEnd, flowProject}] =
  if flowEnd < endOfDay
    then
      if flowEnd == flowStart
        then [end {flowEnd = endOfDay}]
        else [end, FlowView {flowStart = flowEnd, flowEnd = endOfDay, viewType = Other, flowProject}]
    else [end]
normalizeLastView endOfDay (v : rest@(_ : _)) = v : normalizeLastView endOfDay rest

instance HasSummary FlowView FlowType where
  summarize views =
    views
      |> List.sortBy (compare `on` viewType)
      |> NE.groupBy ((==) `on` viewType)
      |> fmap (\flows@(f :| _) -> (viewType f, sum $ fmap duration flows))

duration :: FlowView -> NominalDiffTime
duration FlowView {flowStart, flowEnd} = diffLocalTime flowEnd flowStart

instance HasSummary FlowView ProjectName where
  summarize views =
    views
      |> List.sortBy (compare `on` flowProject)
      |> NE.groupBy ((==) `on` flowProject)
      |> fmap (\flows@(f :| _) -> (flowProject f, sum $ fmap duration flows))

makeSummary :: Maybe LocalTime -> Maybe LocalTime -> [FlowView] -> [CommandView] -> FlowSummary
makeSummary fromTime toTime views commands =
  let summaryFlows =
        views
          |> filter (flowInPeriod fromTime toTime)
          |> summarize
      summaryCommands =
        commands
          |> filter (commandInPeriod fromTime toTime)
          |> summarize
      summaryProjects =
        views
          |> filter (projectInPeriod fromTime toTime)
          |> summarize
      summaryPeriod = makePeriod fromTime toTime
   in FlowSummary {..}
