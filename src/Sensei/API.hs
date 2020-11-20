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

module Sensei.API
  ( SenseiAPI,
    KillServer,
    senseiAPI,
    module Sensei.Flow,
    module Sensei.FlowView,
    module Sensei.Group,
    module Sensei.Utils,
    GroupViews (..),
    Trace (..),
    Group (..),
    UserProfile (..), defaultProfile
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (String),
    withText,
  )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
  (TimeOfDay(..), hoursToTimeZone,  Day,
    NominalDiffTime,
    TimeOfDay,
    TimeZone,
  )
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import GHC.Generics (Generic)
import Sensei.Flow
import Sensei.FlowView
import Sensei.Group
import Sensei.Utils
import Servant

-- * API

type KillServer =
  Summary "Ask the server to kill itself." :> "kill"
    :> Delete '[JSON] ()

type PostRecordTrace =
  Summary "Record execution 'trace' of a single command execution." :> "trace"
    :> ReqBody '[JSON] Trace
    :> Post '[JSON] ()

type PostRecordFlow =
  Summary "Record start of some type of Flow."
    :> Capture "user" Text
    :> Capture "flowType" FlowType
    :> ReqBody '[JSON] FlowState
    :> Post '[JSON] ()

type GetGroupSummary =
  Summary "Retrieve grouped summary of flows by type."
    :> Capture "user" Text
    :> "summary"
    :> Get '[JSON] [GroupViews (FlowType, NominalDiffTime)]

type GetDailySummary =
  Summary "Retrieve daily summary of time spend in flows by type."
    :> Capture "user" Text
    :> Capture "day" Day
    :> "summary"
    :> Get '[JSON] [(FlowType, NominalDiffTime)]

type GetNotes =
  Summary "Retrieve timestamped notes for some day, or all notes if no day is given."
    :> Capture "user" Text
    :> Capture "day" Day
    :> "notes"
    :> Get '[JSON] [NoteView]

type GetCommands =
  Summary "Retrieve sequence of executed commands for some day."
    :> Capture "user" Text
    :> Capture "day" Day
    :> "commands"
    :> Get '[JSON] [CommandView]

type GetFlowsTimeline =
  Summary "Retrieve timeline of flows for a given day."
    :> Capture "user" Text
    :> Capture "day" Day
    :> Get '[JSON] [FlowView]

type GetGroupedTimelines =
  Summary "Retrieve timeline of flows, grouped by some time slice (Day, Week, Month...)."
    :> Capture "user" Text
    :> QueryParams "group" Group
    :> Get '[JSON] [GroupViews FlowView]

type GetUserProfile =
  Summary "Retrieve a user's profile." :> Capture "user" Text :> Get '[JSON] UserProfile

type PutUserProfile =
  Summary "Define current user's profile." :> Capture "user" Text :> ReqBody '[JSON] UserProfile :> Put '[JSON] NoContent

type SenseiAPI =
  PostRecordTrace
    :<|> "flows"
      :> ( PostRecordFlow
             :<|> GetGroupSummary
             :<|> GetDailySummary
             :<|> GetNotes
             :<|> GetCommands
             :<|> GetFlowsTimeline
             :<|> GetGroupedTimelines
         )
    :<|> "users"
      :> (GetUserProfile :<|> PutUserProfile)

senseiAPI :: Proxy SenseiAPI
senseiAPI = Proxy

data UserProfile = UserProfile
  { userName :: Text,
    userTimezone :: TimeZone,
    userStartOfDay :: TimeOfDay,
    userEndOfDay :: TimeOfDay,
    userFlowTypes :: Maybe [FlowType]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

defaultProfile :: UserProfile
defaultProfile =
  UserProfile { userName = "arnaud", userTimezone = hoursToTimeZone 1, userStartOfDay = TimeOfDay 08 00 00 , userEndOfDay = TimeOfDay 18 30 00, userFlowTypes = Nothing  }

instance ToJSON TimeZone where
  toJSON = String . Text.pack . iso8601Show

instance FromJSON TimeZone where
  parseJSON = withText "TimeZone" $ iso8601ParseM . Text.unpack
