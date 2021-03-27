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
    SetCurrentTime,
    GetCurrentTime,
    senseiAPI,
    nextPageLink,
    previousPageLink,
    nextDayLink,
    previousDayLink,
    module Sensei.Color,
    module Sensei.Duration,
    module Sensei.Flow,
    module Sensei.Summary,
    module Sensei.FlowView,
    module Sensei.Group,
    module Sensei.User,
    module Sensei.Utils,
    GroupViews (..),
    Event (..),
    Group (..),
    UserProfile (..),
  )
where

import Control.Applicative ((<|>))
import Data.Text (Text, pack, unpack)
import Data.Time
import Network.HTTP.Link as Link
import Network.URI.Extra (uriFromString)
import Sensei.Color
import Sensei.Duration
import Sensei.Flow
import Sensei.FlowView
import Sensei.Group
import Sensei.Server.Tags
import Sensei.Summary
import Sensei.Time
import Sensei.User
import Sensei.Utils
import Sensei.Version
import Servant

-- * API

type KillServer =
  Summary "Ask the server to kill itself."
    :> Tags "Development"
    :> "kill"
    :> Delete '[JSON] ()

type SetCurrentTime =
  Summary "Set the current time of the server."
    :> Tags "Development"
    :> "time"
    :> Capture "user" Text
    :> ReqBody '[JSON] Timestamp
    :> Put '[JSON] ()

type GetCurrentTime =
  Summary "Get the current time of the server."
    :> Tags "Development"
    :> "time"
    :> Capture "user" Text
    :> Get '[JSON] Timestamp

type DisplayVersions =
  Summary "Get the server executable and storage versions "
    :> "versions"
    :> Get '[JSON] Versions

type PostEvent =
  Summary "Record a new `Event` in the log."
    :> ReqBody '[JSON] Event
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
    :> Patch '[JSON] Event

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
    :> Get '[JSON] (Headers '[Header "Link" Text] [NoteView])

type SearchNotes =
  Summary "Run a full-text search on all notes, retrieving matching notes."
    :> Capture "user" Text
    :> QueryParam "search" Text
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
    :> Get '[JSON] (Maybe Event)

type GetFlowsTimeline =
  Summary "Retrieve timeline of flows for a given day."
    :> Capture "user" Text
    :> Capture "day" Day
    :> Get '[JSON] [FlowView]

type GetGroupedTimelines =
  Summary
    "Retrieve timeline of flows, grouped by some time slice (Day, Week, Month...). \
    \ If no 'group' param is given, returns _all_ flows."
    :> Capture "user" Text
    :> QueryParams "group" Group
    :> Get '[JSON] [GroupViews FlowView]

type GetAllLog =
  Summary "Retrieve the complete log of all events pertaining to a given user, most recent first"
    :> Capture "user" Text
    :> QueryParam "page" Natural
    :> Get '[JSON] (Headers '[Header "Link" Text] [Event])

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
  "api"
    :> ( "flows"
           :> Tags "Flows"
           :> ( GetFlow
                  :<|> PatchFlowTimeshift
                  :<|> GetGroupSummary
                  :<|> GetDailySummary
                  :<|> GetNotes
                  :<|> GetCommands
                  :<|> GetFlowsTimeline
                  :<|> GetGroupedTimelines
              )
           :<|> "notes"
             :> Tags "Notes"
             :> SearchNotes
           :<|> "log"
             :> Tags "Event Log"
             :> ( PostEvent
                    :<|> GetAllLog
                )
           :<|> "users"
             :> Tags "User Profile"
             :> (GetUserProfile :<|> PutUserProfile)
           :<|> Tags "Metadata" :> DisplayVersions
       )

senseiAPI :: Proxy SenseiAPI
senseiAPI = Proxy

nextPageLink :: Text -> Maybe Natural -> Maybe Link.Link
nextPageLink userName page = do
  p <- page <|> pure 1
  let next = show (succ p)
  uri <- uriFromString $ "/api/log/" <> unpack userName <> "?page=" <> next
  pure $ Link uri [(Rel, "next"), (Link.Other "page", pack next)]

previousPageLink :: Text -> Maybe Natural -> Maybe Link.Link
previousPageLink userName page = do
  p <- page
  let prev = show (pred p)
  uri <- uriFromString $ "/api/log/" <> unpack userName <> "?page=" <> prev
  pure $ Link uri [(Rel, "prev"), (Link.Other "page", pack prev)]

nextDayLink :: Text -> Maybe Day -> Maybe Link.Link
nextDayLink userName day = do
  d <- day
  let next = showGregorian (succ d)
  uri <- uriFromString $ "/api/flows/" <> unpack userName <> "/" <> next <> "/" <> "notes"
  pure $ Link uri [(Rel, "next"), (Link.Other "page", pack next)]

previousDayLink :: Text -> Maybe Day -> Maybe Link.Link
previousDayLink userName day = do
  d <- day
  let prev = showGregorian (pred d)
  uri <- uriFromString $ "/api/flows/" <> unpack userName <> "/" <> prev <> "/" <> "notes"
  pure $ Link uri [(Rel, "prev"), (Link.Other "page", pack prev)]
