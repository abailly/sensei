{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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

data UserProfile = UserProfile
  { userName :: Text,
    userTimezone :: TimeZone,
    userStartOfDay :: TimeOfDay,
    userEndOfDay :: TimeOfDay,
    userFlowTypes :: Maybe (Map.Map FlowType Color),
    userProfileVersion :: Natural
  }
  deriving (Eq, Show, Generic, ToJSON)

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
      userFlowTypes = Nothing,
      userProfileVersion = currentVersion
    }

parseJSONFromVersion :: Natural -> Object -> Parser UserProfile
parseJSONFromVersion v o =
  UserProfile
    <$> o .: "userName"
    <*> o .: "userTimezone"
    <*> o .: "userStartOfDay"
    <*> o .: "userEndOfDay"
    <*>
    (if v < 2
     then addDefaultColorsToFlowTypes
     else o .: "userFlowTypes")
    <*> case v of
      0 -> pure currentVersion
      _ -> o .: "userProfileVersion"
  where
    addDefaultColorsToFlowTypes = do
      flowTypes <- o .: "userFlowTypes"
      pure $ Just $ Map.fromList (zip flowTypes (randomColors 12))

instance FromJSON UserProfile where
  parseJSON =
    withObject "UserProfile" $ \o -> do
      version <- (o .: "userProfileVersion") <|> pure 0
      parseJSONFromVersion version o

instance ToJSON TimeZone where
  toJSON = String . Text.pack . iso8601Show

instance FromJSON TimeZone where
  parseJSON = withText "TimeZone" $ iso8601ParseM . Text.unpack
