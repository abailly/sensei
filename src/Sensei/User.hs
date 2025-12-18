{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Sensei.User where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import GHC.Generics (Generic)
import Numeric.Natural
import Preface.Codec (Base64, Encoded, Hex)
import Sensei.Backend (Backend (..))
import Sensei.Bsky ()
import Sensei.Bsky.Core (parseBskyBackendFromVersion)
import Sensei.Color
import Sensei.FlowType (FlowType)
import Sensei.Project (ProjectName, Regex)
import Sensei.Time (TZLabel (..))
import Sensei.Version (currentVersion)
import Servant (FromHttpApiData, ToHttpApiData)

newtype UserName = UserName {unUserName :: Text}
  deriving newtype (Eq, Show, IsString, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

-- | Customizable parameters for registering and displaying flows.
--   This configuration defines user-specific configurations that are used
--   by various parts of /sensei/ to handle data.
data UserProfile = UserProfile
  { -- | The user name, or alias, used to display profile and identify the user
    --  in teams context
    userName :: Text,
    -- | The local timezone this user wants her data to be displayed in.
    --  All events are recorded and stored by the server in `UTCTime` (Universal Time Coordinates)
    --  but for display and analysis purpose we need to be able to relate absolute timestamps
    --  to meaningful time. In serialised form, this should be something like 'Europe/Paris' or
    -- 'Africa/Abidjan'.
    userTimezone :: TZLabel,
    -- | This user's standard start of day time. This parameter is used when displaying timelines
    --  for flows and other events, to bound the scale the timeline is displayed with. It's also
    --  used when normalising and grouping flows in a timeline.
    userStartOfDay :: TimeOfDay,
    -- | This user's standard end of (work) day time. This is used to normalise daily timelines, either to add
    --  some dummy flow or to compute the end of flows which have not been properly "closed".
    --  If one forgets to `End` the day then this parameter will be used to limit the duration of "unfinished"
    --  flows.
    userEndOfDay :: TimeOfDay,
    -- | Custom definition of `FlowType`s for this user.
    --  A user can define her or his own `FlowType` identifiers. Those will be used throughout the system
    --  and can be further customised by associating a `Color` with them
    userFlowTypes :: Maybe (Map.Map FlowType Color),
    -- | Custom definition of command aliases
    --  This maps an alias to an actual, usually absolute, command path. When `ep` is invoked as the alias,
    --  it actually will wrap referenced program's execution in the current environment.
    userCommands :: Maybe (Map.Map String String),
    -- | Custom definition of projects
    -- This maps a regular expression to some project identifier. The regular expression is used to assign
    -- traces to projects based on the directory they are tied to.
    userProjects :: Map.Map Regex ProjectName,
    -- | User's password, salted and hashed.
    -- The profile stores the user's password properly salted and hashed with bcrypt.
    userPassword :: (Encoded Base64, Encoded Base64),
    -- | The user unique identifier.
    -- This is an hexadecimal string representing 16 bytes
    userId :: Encoded Hex,
    -- | List of additional backends configured for this user.
    -- Events posted will be dispatched to each configured backend.
    backends :: [Backend]
  }
  deriving (Eq, Show, Generic)

userDefinedFlows :: UserProfile -> Maybe [FlowType]
userDefinedFlows UserProfile {userFlowTypes} =
  Map.keys <$> userFlowTypes

defaultProfile :: UserProfile
defaultProfile =
  UserProfile
    { userName = "arnaud",
      userTimezone = Europe__Paris,
      userStartOfDay = TimeOfDay 08 00 00,
      userEndOfDay = TimeOfDay 18 30 00,
      userFlowTypes = Nothing,
      userCommands = Nothing,
      userPassword = ("", ""),
      userProjects = mempty,
      userId = "",
      backends = []
    }

instance ToJSON UserProfile where
  toJSON UserProfile {..} =
    object
      [ "userName" .= userName,
        "userTimezone" .= userTimezone,
        "userStartOfDay" .= userStartOfDay,
        "userEndOfDay" .= userEndOfDay,
        "userFlowTypes" .= userFlowTypes,
        "userCommands" .= userCommands,
        "userPassword" .= userPassword,
        "userId" .= userId,
        "userProjects" .= userProjects,
        "userProfileVersion" .= currentVersion,
        "backends" .= backends
      ]

instance FromJSON UserProfile where
  parseJSON =
    withObject "UserProfile" $ \o -> do
      version <- (o .: "userProfileVersion") <|> pure 0
      parseJSONFromVersion version o

parseJSONFromVersion :: Natural -> Object -> Parser UserProfile
parseJSONFromVersion v o =
  UserProfile
    <$> o .: "userName"
    <*> parseTimeZone
    <*> o .: "userStartOfDay"
    <*> o .: "userEndOfDay"
    <*> parseFlowTypes
    <*> parseCommands
    <*> parseProjects
    <*> parsePassword
    <*> parseId
    <*> parseBackends
  where
    parseFlowTypes =
      case v of
        2 ->
          ((o .: "userFlowTypes") <&> (Just . Map.fromList))
            <|> fail ("Cannot decode version 2 'userFlowTypes' at version " <> show currentVersion <> ", expected an array of tuples")
        1 ->
          o .: "userFlowTypes"
            >>= \flowTypes -> pure $ Just $ Map.fromList (zip flowTypes (randomColors 12))
        0 -> pure Nothing
        _ -> o .: "userFlowTypes"

    parseCommands =
      if v < 4
        then pure Nothing
        else o .: "userCommands"

    parsePassword =
      if v <= 5
        then pure ("", "")
        else o .: "userPassword"

    parseId =
      if v <= 6
        then pure ""
        else o .: "userId"

    parseProjects =
      if v <= 7
        then pure mempty
        else o .: "userProjects"

    parseTimeZone =
      if v <= 9
        then pure Europe__Paris
        else o .: "userTimezone"

    parseBackends =
      if v < 11
        then pure []
        else o .: "backends" >>= traverse parseBackend

    parseBackend obj = Backend <$> parseBskyBackendFromVersion v obj -- NOTE: the intention here is that we can add more backends in the future, using alternative parsers to try and parse each one

instance ToJSON TimeZone where
  toJSON = String . Text.pack . iso8601Show

instance FromJSON TimeZone where
  parseJSON = withText "TimeZone" $ iso8601ParseM . Text.unpack
