{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
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
import Data.Time
import GHC.Generics
import Sensei.Flow
import Sensei.Summary
import Sensei.Time
import Sensei.Utils

-- | A single note
data NoteView = NoteView
  { noteStart :: LocalTime,
    noteView :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | A view of an executed command
data CommandView = CommandView
  { commandStart :: LocalTime,
    commandProcess :: Text,
    commandElapsed :: NominalDiffTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance HasSummary CommandView Text where
  summarize commands =
    commands
      |> List.sortBy (compare `on` commandProcess)
      |> NE.groupBy ((==) `on` commandProcess)
      |> fmap (\cmds@(f :| _) -> (commandProcess f, sum $ fmap commandElapsed cmds))

mkCommandView ::
  TimeZone -> Event -> Maybe CommandView
mkCommandView tz (EventTrace Trace {..}) =
  Just
    CommandView
      { commandStart = utcToLocalTime tz _traceTimestamp,
        commandProcess = _traceProcess,
        commandElapsed = _traceElapsed
      }
mkCommandView _ _ = Nothing

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
    viewType :: FlowType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

flowInPeriod ::
  Maybe LocalTime -> Maybe LocalTime -> FlowView -> Bool
flowInPeriod (Just lb) (Just ub) = withinPeriod lb ub flowStart
flowInPeriod _ _ = undefined

appendFlow :: TimeZone -> TimeOfDay -> Event -> [FlowView] -> [FlowView]
appendFlow _ _ (EventFlow (Flow {_flowType = End})) [] = []
appendFlow tz _ (EventFlow (Flow {_flowType = End, ..})) (v : vs) =
  v {flowEnd = utcToLocalTime tz _flowTimestamp} : vs
appendFlow tz dayEnd (EventFlow (Flow {..})) views =
  let view = FlowView st st _flowType
      st = utcToLocalTime tz _flowTimestamp
   in case views of
        (v : vs) -> view : fillFlowEnd dayEnd v st : vs
        [] -> [view]
appendFlow _ _ _ views = views

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
  [FlowView startOfDay endOfDay Other]
normalizeViewsForDay startOfDay endOfDay views@(st : _) =
  let st' =
        if flowStart st > startOfDay
          then [FlowView startOfDay (flowStart st) Other]
          else []
      end' = normalizeLastView endOfDay views
   in st' <> end'

normalizeLastView :: LocalTime -> [FlowView] -> [FlowView]
normalizeLastView _ [] = []
normalizeLastView endOfDay [end] =
  if flowEnd end < endOfDay
    then
      if flowEnd end == flowStart end
        then [end {flowEnd = endOfDay}]
        else [end, FlowView (flowEnd end) endOfDay Other]
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
      summaryPeriod = makePeriod fromTime toTime
   in FlowSummary {..}
