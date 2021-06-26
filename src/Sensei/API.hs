{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.API
  ( SenseiAPI,
    KillServer,
    LoginAPI,
    SetCurrentTime,
    GetCurrentTime,
    Protected,
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
    Event (..),
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
import Sensei.Time
import Sensei.User
import Sensei.Utils
import Sensei.Version
import Sensei.Server.Auth.Types (Credentials, SetCookie, Auth, JWT, Cookie, AuthenticationToken)
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

type GetPeriodSummary =
  Summary
    "Retrieve flows summary, eg. time spend in flows by type, for a given time period. \
    \ The time period is given by query arguments `from` and `to`, with `from` being \
    \ inclusive lower bound and `to` being exclusive upper bound. \
    \ `from` must be a valid ISO8601 date _before_ `to`."
    :> Capture "user" Text
    :> "summary"
    :> QueryParam "from" Day
    :> QueryParam "to" Day
    :> QueryParam "period" Group
    :> Get '[JSON] (Headers '[Header "Link" Text] FlowSummary)

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

type LoginAPI =
  Summary "Allows users to login passing in credentials."
    :> Description "If successful, this will set cookies containing user's data in the form of JWT token.\
                   \ It will also return the 'UserProfile' of the logged in user."
    
    :> "login"
    :> ReqBody '[JSON] Credentials
    :> Post
         '[JSON]
         ( Headers
             '[ Header "Set-Cookie" SetCookie,
                Header "Set-Cookie" SetCookie
              ]
             UserProfile
         )

-- | Protected endpoints require a valid 'AuthenticationToken' which can be provided
-- through a 'JWT' token, or a 'Cookie'
type Protected = Auth '[JWT, Cookie] AuthenticationToken

type SenseiAPI =
  "api"
    :> ( "flows"
           :> Tags "Flows"
           :> ( GetFlow
                  :<|> PatchFlowTimeshift
                  :<|> GetPeriodSummary
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
