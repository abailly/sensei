{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.FlowView where

import Data.Aeson hiding (Options)
import Data.Function (on)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Time
import GHC.Generics
import Sensei.Flow
import Sensei.Utils


-- | A single "flow" timeslice of a given type
-- `FlowView`s times are expressed in the `LocalTime` of the users to which they
-- pertain.
data FlowView = FlowView
  { flowStart :: LocalTime,
    flowEnd :: LocalTime,
    flowType :: FlowType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

appendFlow :: TimeZone -> TimeOfDay -> Flow -> [FlowView] -> [FlowView]
appendFlow _ _ Flow {_flowType = Note} views = views
appendFlow _ _ Flow {_flowType = End} [] = []
appendFlow tz _ Flow {_flowType = End, _flowState} (v : vs) =
  v {flowEnd = utcToLocalTime tz $ _flowStart _flowState} : vs
appendFlow tz dayEnd Flow {..} views =
      let view = FlowView st st _flowType
          st = utcToLocalTime tz $ _flowStart _flowState
       in case views of
            (v : vs) -> view : fillFlowEnd dayEnd v st : vs
            [] -> [view]

-- OUCH
fillFlowEnd :: TimeOfDay -> FlowView -> LocalTime -> FlowView
fillFlowEnd endOfDay v st
  |  flowStart v == flowEnd v =
     if localDay st == localDay (flowStart v)
     then v {flowEnd = st}
     else
       let end = LocalTime (localDay (flowStart v)) endOfDay
           oneHour = secondsToNominalDiffTime 3600
           plus1hour = addLocalTime oneHour (flowStart v)
       in if end < (flowStart v)
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
  then if flowEnd end == flowStart end
       then [end { flowEnd = endOfDay}]
       else [end, FlowView (flowStart end) endOfDay Other]
  else [end]
normalizeLastView endOfDay (v : rest@(_:_)) = v : normalizeLastView endOfDay rest

summarize :: [FlowView] -> [(FlowType, NominalDiffTime)]
summarize views =
  views
    |> List.sortBy (compare `on` flowType)
    |> NE.groupBy ((==) `on` flowType)
    |> fmap (\flows@(f :| _) -> (flowType f, sum $ fmap duration flows))

duration :: FlowView -> NominalDiffTime
duration FlowView{flowStart, flowEnd} = diffLocalTime flowEnd flowStart
