{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sensei.User where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import GHC.Generics (Generic)
import Numeric.Natural
import Sensei.Color
import Sensei.Flow

-- |Customizable parameters for registering and displaying flows.
-- This configuration defines user-specific configurations that are used
-- by various parts of /sensei/ to handle data.
data UserProfile = UserProfile
  { userName :: Text,
    -- ^The user name, or alias, used to display profile and identify the user
    -- in teams context
    userTimezone :: TimeZone,
    -- ^The local timezone this user wants her data to be displayed in.
    -- All events are recorded and stored by the server in `UTCTime` (Universal Time Coordinates)
    -- but for display and analysis purpose we need to be able to relate absolute timestamps
    -- with meaningful time.
    userStartOfDay :: TimeOfDay,
    -- ˆThis user's standard start of day time. This parameter is used when displaying timelines
    -- for flows and other events, to bound the scale the timeline is displayed with. It's also
    -- used when normalising and grouping flows in a timeline.
    userEndOfDay :: TimeOfDay,
    -- ^This user's standard end of (work) day time. This is used to normalise daily timelines, either to add
    -- some dummy flow or to compute the end of flows which have not been properly "closed".
    -- If one forgets to `End` the day then this parameter will be used to limit the duration of "unfinished"
    -- flows.
    userFlowTypes :: Maybe (Map.Map FlowType Color)
    -- ^Custom definition of `FlowType`s for this user.
    -- A user can define her or his own `FlowType` identifiers. Those will be used throughout the system
    -- and can be further customised by associating a `Color` with them
  }
  deriving (Eq, Show, Generic)

userDefinedFlows :: UserProfile -> Maybe [FlowType]
userDefinedFlows UserProfile {userFlowTypes} =
  Map.keys <$> userFlowTypes

defaultProfile :: UserProfile
defaultProfile =
  UserProfile
    { userName = "arnaud",
      userTimezone = hoursToTimeZone 1,
      userStartOfDay = TimeOfDay 08 00 00,
      userEndOfDay = TimeOfDay 18 30 00,
      userFlowTypes = Nothing
    }

parseJSONFromVersion :: Natural -> Object -> Parser UserProfile
parseJSONFromVersion v o =
  UserProfile
    <$> o .: "userName"
    <*> o .: "userTimezone"
    <*> o .: "userStartOfDay"
    <*> o .: "userEndOfDay"
    <*> parseFlowTypes
   where
    parseFlowTypes =
      case v of
        3 -> o .: "userFlowTypes"
        2 -> o .: "userFlowTypes" >>= pure . Just . Map.fromList
        1 -> o .: "userFlowTypes" >>=
              \ flowTypes -> pure $ Just $ Map.fromList (zip flowTypes (randomColors 12))
        _ -> pure Nothing

instance ToJSON UserProfile where
  toJSON UserProfile{..} =
    object [ "userName" .= userName,
             "userTimezone" .= userTimezone,
             "userStartOfDay" .= userStartOfDay,
             "userEndOfDay" .= userStartOfDay,
             "userFlowTypes" .= userFlowTypes,
             "userProfileVersion" .= currentVersion
           ]

instance FromJSON UserProfile where
  parseJSON =
    withObject "UserProfile" $ \o -> do
      version <- (o .: "userProfileVersion") <|> pure 0
      parseJSONFromVersion version o

instance ToJSON TimeZone where
  toJSON = String . Text.pack . iso8601Show

instance FromJSON TimeZone where
  parseJSON = withText "TimeZone" $ iso8601ParseM . Text.unpack
