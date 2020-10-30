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

module Sensei.API where

import Data.Aeson hiding (Options)
import Data.Function (on, (&))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Data.Time
import GHC.Generics
import Servant
import System.Exit

-- * API

type SenseiAPI =
  "trace" :> ReqBody '[JSON] Trace :> Post '[JSON] ()
    :<|> "flows"
      :> ( Capture "flowType" FlowType :> ReqBody '[JSON] FlowState :> Post '[JSON] ()
             :<|> Capture "user" String :> "summary" :> Get '[JSON] [GroupViews (FlowType, NominalDiffTime)]
             :<|> Capture "user" String :> Capture "day" Day :> "summary" :> Get '[JSON] [(FlowType, NominalDiffTime)]
             :<|> Capture "user" String :> Capture "day" Day :> "notes" :> Get '[JSON] [(UTCTime, Text.Text)]
             :<|> Capture "user" String :> Capture "day" Day :> Get '[JSON] [FlowView]
             :<|> Capture "user" String :> QueryParams "group" Group :> Get '[JSON] [GroupViews FlowView]
         )
    :<|> Raw

-- | Execution "trace" of a program
data Trace = Trace
  { timestamp :: UTCTime,
    directory :: FilePath,
    process :: String,
    args :: [String],
    exit_code :: ExitCode,
    elapsed :: NominalDiffTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

deriving instance ToJSON ExitCode

deriving instance FromJSON ExitCode

-- | A single flow for a given user
data FlowView = FlowView
  { flowStart :: UTCTime,
    flowEnd :: UTCTime,
    duration :: NominalDiffTime,
    flowType :: FlowType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- OUCH
fillFlowEnd :: FlowView -> UTCTime -> FlowView
fillFlowEnd v st =
  if utctDay st == utctDay (flowStart v)
    then v {flowEnd = st, duration = diffUTCTime st (flowStart v)}
    else
      let end = UTCTime (utctDay (flowStart v)) endOfWorkDay
          oneHour = secondsToNominalDiffTime 3600
          plus1hour = addUTCTime oneHour (flowStart v)
       in if end < (flowStart v)
            then v {flowEnd = plus1hour, duration = oneHour}
            else v {flowEnd = end, duration = diffUTCTime end (flowStart v)}

-- | Normalize the given list of views to ensure it starts and ends at "standard" time.
-- This function is useful to provide a common scale when comparing flows across several days
-- which might not start nor end at the same time.
-- This function assumes that:
-- 1. The given `FlowView` list is sorted
-- 2. All `FLowView` are for the same day
normalizeViewsForDay :: UTCTime -> UTCTime -> [FlowView] -> [FlowView]
normalizeViewsForDay startOfDay endOfDay [] =
  [FlowView startOfDay endOfDay (diffUTCTime endOfDay startOfDay) Other]
normalizeViewsForDay startOfDay endOfDay views@(st : _) =
  let end = last views
      st' =
        if flowStart st > startOfDay
          then [FlowView startOfDay (flowStart st) (diffUTCTime startOfDay (flowStart st)) Other]
          else []
      end' =
        if flowEnd end < endOfDay
          then [FlowView (flowEnd end) endOfDay (diffUTCTime (flowEnd end) endOfDay) Other]
          else []
   in st' <> views <> end'

summarize :: [FlowView] -> [(FlowType, NominalDiffTime)]
summarize views =
  views
    |> List.sortBy (compare `on` flowType)
    |> NE.groupBy ((==) `on` flowType)
    |> fmap (\ flows@(f :| _) -> (flowType f, sum $ fmap duration flows))

sameDayThan :: Day -> (a -> UTCTime) -> a -> Bool
sameDayThan day selector a =
  utctDay (selector a) == day

senseiAPI :: Proxy SenseiAPI
senseiAPI = Proxy

data FlowType = Learning | Experimenting | Troubleshooting | Flowing | Rework | Note | Other | Meeting | End
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

instance ToHttpApiData FlowType where
  toUrlPiece f = Text.pack (show f)

instance FromHttpApiData FlowType where
  parseUrlPiece "Learning" = pure Learning
  parseUrlPiece "Experimenting" = pure Experimenting
  parseUrlPiece "Troubleshooting" = pure Troubleshooting
  parseUrlPiece "Flowing" = pure Flowing
  parseUrlPiece "Rework" = pure Rework
  parseUrlPiece "Note" = pure Note
  parseUrlPiece "End" = pure End
  parseUrlPiece "Meeting" = pure Meeting
  parseUrlPiece _txt = pure Other

data FlowState
  = FlowState
      { _flowUser :: String,
        _flowStart :: UTCTime,
        _flowDir :: String
      }
  | FlowNote
      { _flowUser :: String,
        _flowStart :: UTCTime,
        _flowDir :: String,
        _flowNote :: Text.Text
      }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Flow = Flow
  { _flowType :: FlowType,
    _flowState :: FlowState
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Grouping of `FlowView`
data Group = Day | Week | Month | Quarter | Year
  deriving (Eq, Read, Show, Ord, Generic, ToJSON, FromJSON)

instance ToHttpApiData Group where
  toUrlPiece f = Text.pack (show f)

instance FromHttpApiData Group where
  parseUrlPiece txt =
    case reads (Text.unpack txt) of
      ((g, _) : _) -> pure g
      _ -> Left $ "cannot parse group " <> txt

data GroupViews a
  = NoViews
  | Leaf {leafViews :: [a]}
  | GroupLevel {level :: Group, groupTime :: UTCTime, subGroup :: GroupViews a}
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Functor)

groupViews :: [Group] -> [FlowView] -> [GroupViews FlowView]
groupViews [] views = [Leaf views]
groupViews (Day : _groups) views =
  views
    |> NE.groupBy ((==) `on` (utctDay . flowStart))
    |> mkGroupViewsBy Day
groupViews _ _ = error "unsupported group"

mkGroupViewsBy :: Group -> [NE.NonEmpty FlowView] -> [GroupViews FlowView]
mkGroupViewsBy Day =
  fmap mkGroup
  where
    normalized :: NE.NonEmpty FlowView -> [FlowView]
    normalized (view :| rest) =
      let viewDay = utctDay (flowStart view)
       in normalizeViewsForDay (UTCTime viewDay startOfWorkDay) (UTCTime viewDay endOfWorkDay) (view : rest)

    mkGroup :: NE.NonEmpty FlowView -> GroupViews FlowView
    mkGroup (view :| rest) = GroupLevel Day (flowStart view) (Leaf (normalized (view :| rest)))
mkGroupViewsBy _ = error "unsupported group"

-- | End of work day is assumed to be 6:30pm UTC
-- TODO fix this value which is incorrect and locale dependent
startOfWorkDay :: DiffTime
startOfWorkDay = secondsToDiffTime (3600 * 8)

-- | End of work day is assumed to be 6:30pm UTC
-- TODO fix this value which is incorrect and locale dependent
endOfWorkDay :: DiffTime
endOfWorkDay = secondsToDiffTime (3600 * 17 + 1800)

-- | "pipe" operator common in other languages
-- this is basically `flip apply` which is defined as `&` in
-- the standard library
(|>) :: a -> (a -> b) -> b
(|>) = (&)
