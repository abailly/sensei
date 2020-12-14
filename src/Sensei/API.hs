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
    module Sensei.Color,
    module Sensei.Duration,
    module Sensei.Flow,
    module Sensei.Summary,
    module Sensei.FlowView,
    module Sensei.Group,
    module Sensei.User,
    module Sensei.Utils,
    GroupViews (..),
    Trace (..),
    Group (..),
    UserProfile (..),
  )
where

import Data.Text (Text)
import Data.Time
import Sensei.Color
import Sensei.Duration
import Sensei.Flow
import Sensei.FlowView
import Sensei.Group
import Sensei.Server.Tags
import Sensei.Summary
import Sensei.User
import Sensei.Utils
import Sensei.Version
import Servant

-- * API

type KillServer =
  Summary "Ask the server to kill itself." :> "kill"
    :> Delete '[JSON] ()

type DisplayVersions =
  Summary "Get the server executable and storage versions "
    :> "versions"
    :> Get '[JSON] Versions

type PostRecordTrace =
  Summary "Record execution 'trace' of a single command execution."
    :> "trace"
    :> ReqBody '[JSON] Trace
    :> Post '[JSON] ()

type PostRecordFlow =
  Summary "Record start of some type of Flow."
    :> Capture "user" Text
    :> Capture "flowType" FlowType
    :> ReqBody '[JSON] FlowState
    :> Post '[JSON] ()

type PatchFlowTimeshift =
  Summary
    "Changes the latest recorded flow's start time by some amount of time. \
    \If the resulting timestamp happens before the previous flow's start time, \
    \it raises an error. \
    \Returns the updated Flow."
    :> Capture "user" Text
    :> "latest"
    :> "timestamp"
    :> ReqBody '[JSON] TimeDifference
    :> Patch '[JSON] FlowState

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
    :> Get '[JSON] FlowSummary

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

type GetFlow =
  Summary "Query flows"
    :> Capture "user" Text
    :> Capture "ref" Reference
    :> Get '[JSON] (Maybe FlowView)

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

type GetAllLog =
  Summary "Retrieve the complete log of all events pertaining to a given user, most recent first"
    :> Capture "user" Text
    :> Get '[JSON] [Event]

type GetUserProfile =
  Summary "Retrieve a user's profile."
    :> Capture "user" Text
    :> Get '[JSON] UserProfile

type PutUserProfile =
  Summary "Define current user's profile."
    :> Capture "user" Text
    :> ReqBody '[JSON] UserProfile
    :> Put '[JSON] NoContent

type SenseiAPI =
  Tags "Flows" :> PostRecordTrace
    :<|> "flows"
      :> Tags "Flows"
      :> ( PostRecordFlow
             :<|> GetFlow
             :<|> PatchFlowTimeshift
             :<|> GetGroupSummary
             :<|> GetDailySummary
             :<|> GetNotes
             :<|> GetCommands
             :<|> GetFlowsTimeline
             :<|> GetGroupedTimelines
         )
    :<|> "log"
      :> Tags "Event Log"
      :> GetAllLog
    :<|> "users"
      :> Tags "User Profile"
      :> (GetUserProfile :<|> PutUserProfile)
    :<|> Tags "Metadata" :> DisplayVersions

senseiAPI :: Proxy SenseiAPI
senseiAPI = Proxy
