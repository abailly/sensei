{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Sensei.User where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Sensei.Flow
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import GHC.Generics (Generic)
import Numeric.Natural
import Control.Applicative
import Data.Aeson.Types (Parser)

data UserProfile = UserProfile
  { userName :: Text,
    userTimezone :: TimeZone,
    userStartOfDay :: TimeOfDay,
    userEndOfDay :: TimeOfDay,
    userFlowTypes :: Maybe [FlowType],
    userProfileVersion :: Natural
  }
  deriving (Eq, Show, Generic, ToJSON)

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
            <*> o .: "userFlowTypes"
            <*> case v of
                  0 -> pure currentVersion
                  _ -> o .: "userProfileVersion"

instance FromJSON UserProfile where
  parseJSON =
    withObject "UserProfile" $ \ o -> do
      version <- (o .: "userProfileVersion") <|> pure 0
      parseJSONFromVersion version o

instance ToJSON TimeZone where
  toJSON = String . Text.pack . iso8601Show

instance FromJSON TimeZone where
  parseJSON = withText "TimeZone" $ iso8601ParseM . Text.unpack
