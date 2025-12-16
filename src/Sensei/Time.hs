{-# LANGUAGE RecordWildCards #-}

module Sensei.Time (
    Timestamp (..),
    TimeRange (..),
    inRange,
    rangeFromDay,
    withinPeriod,
    module Data.Time,
    module Data.Time.Zones,
    module Data.Time.Zones.All,
) where

import Data.Aeson
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time
import Data.Time.Zones
import Data.Time.Zones.All
import GHC.Generics (Generic)

newtype Timestamp = Timestamp {timestamp :: UTCTime}
    deriving (Eq, Show)

instance ToJSON Timestamp where
    toJSON (Timestamp ts) = object ["timestamp" .= ts]

instance FromJSON Timestamp where
    parseJSON = withObject "Timestamp" $ \o -> Timestamp <$> o .: "timestamp"

{- | A range of time to limit query of flows.
 The lower bound is inclusive, the upper bound exclusive.
-}
data TimeRange = TimeRange
    { rangeStart :: UTCTime
    , rangeEnd :: UTCTime
    }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

inRange :: TimeRange -> UTCTime -> Bool
inRange TimeRange{..} t =
    t >= rangeStart && t <= rangeEnd

-- | Make an absolute `TimeRange` corresponding to given relative `Day`
rangeFromDay :: Day -> TZLabel -> TimeRange
rangeFromDay day tzLabel =
    let tz = tzByLabel tzLabel
        localDay = LocalTime day (TimeOfDay 0 0 0)
        rangeStart = localTimeToUTCTZ tz localDay
        rangeEnd = localTimeToUTCTZ tz $ LocalTime (succ day) (TimeOfDay 0 0 0)
     in TimeRange{..}

withinPeriod :: LocalTime -> LocalTime -> (a -> LocalTime) -> a -> Bool
withinPeriod lowerBound upperBound selector a =
    let time = selector a
     in time >= lowerBound && time < upperBound

instance ToJSON TZLabel where
    toJSON = String . decodeUtf8With lenientDecode . toTZName

instance FromJSON TZLabel where
    parseJSON = withText "TZLabel" $ \t ->
        maybe (fail $ "Unknown TZLabel " <> show t) pure $ fromTZName (encodeUtf8 t)
