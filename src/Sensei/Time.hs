{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sensei.Time
  ( Timestamp (..),
    TimeRange (..),
    inRange,
    rangeFromDay,
    sameDayThan,
    module Data.Time,
  )
where

import Data.Aeson
import Data.Time
import GHC.Generics (Generic)

newtype Timestamp = Timestamp {timestamp :: UTCTime}
  deriving (Eq, Show)

instance ToJSON Timestamp where
  toJSON (Timestamp ts) = object ["timestamp" .= ts]

instance FromJSON Timestamp where
  parseJSON = withObject "Timestamp" $ \o -> Timestamp <$> o .: "timestamp"

-- | A range of time to limit query of flows.
-- The lower bound is inclusive, the upper bound exclusive.
data TimeRange = TimeRange
  { rangeStart :: UTCTime,
    rangeEnd :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

inRange :: TimeRange -> UTCTime -> Bool
inRange TimeRange {..} t =
  t >= rangeStart && t <= rangeEnd

-- | Make an absolute `TimeRange` corresponding to given relative `Day`
rangeFromDay :: Day -> TimeZone -> TimeRange
rangeFromDay day tz =
  let rangeStart = localTimeToUTC tz $ LocalTime day (TimeOfDay 0 0 0)
      rangeEnd = localTimeToUTC tz $ LocalTime (succ day) (TimeOfDay 0 0 0)
   in TimeRange {..}

sameDayThan :: Day -> (a -> Day) -> a -> Bool
sameDayThan day selector a =
  selector a == day
