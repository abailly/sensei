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
    module Sensei.Project,
    module Sensei.Utils,
    GroupViews (..),
    Event (..),
    Group (..),
    UserProfile (..),
  )
where

import Data.Text (Text)
import Data.Time
import Preface.Codec (Encoded, Hex)
import Sensei.Color
import Sensei.Duration
import Sensei.Flow
import Sensei.FlowView
import Sensei.Group
import Sensei.Server.Auth (Auth, AuthenticationToken, Cookie, Credentials, JWT, SerializedToken, SetCookie)
import Sensei.Server.Tags
import Sensei.Summary
import Sensei.Time
import Sensei.User
import Sensei.Utils
import Sensei.Project
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
    :> Capture "user" Text :? "User name to set current time for"
    :> ReqBody '[JSON] Timestamp
    :> Put '[JSON] ()

type GetCurrentTime =
  Summary "Get the current time of the server."
    :> Tags "Development"
    :> "time"
    :> Capture "user" Text :? "User name to get current time of"
    :> Get '[JSON] Timestamp

type DisplayVersions =
  Summary "Get the server executable and storage versions "
    :> "versions"
    :> Get '[JSON] Versions

type PostEvent =
  Summary "Record a new `Event` in the log."
    :> ReqBody '[JSON] [Event]
    :> Post '[JSON] ()

type PatchFlowTimeshift =
  Summary
    "Changes the latest recorded flow's start time by some amount of time. \
    \If the resulting timestamp happens before the previous flow's start time, \
    \it raises an error. \
    \Returns the updated Flow."
    :> Capture "user" Text :? "User to patch latest flow timestamp"
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
    :> Capture "user" Text :? "User to get summary for"
    :> "summary"
    :> QueryParam "from" Day :? "Starting date (in ISO8601 format) for period summary"
    :> QueryParam "to" Day :? "End date (in ISO8601 format) for period summary"
    :> QueryParam "period" Group
      :? "Granularity of summary period, to provide meaningful \
         \ links to next and previous summaries."
    :> Get '[JSON] (Headers '[Header "Link" Text] FlowSummary)

type GetNotes =
  Summary "Retrieve timestamped notes for some day, or all notes if no day is given."
    :> Capture "user" Text :? "User to get notes for"
    :> Capture "day" Day :? "The date to retrieve notes for, in ISO8601 format."
    :> "notes"
    :> Get '[JSON] (Headers '[Header "Link" Text] [NoteView])

type SearchNotes =
  Summary "Run a full-text search on all notes, retrieving matching notes."
    :> Capture "user" Text :? "User to search notes for"
    :> QueryParam "search" Text :? "The search 'expression', ie. some word or sentence fragment"
    :> Get '[JSON] [NoteView]

type GetCommands =
  Summary "Retrieve sequence of executed commands for some day."
    :> Capture "user" Text :? "User to retrieve commands for"
    :> Capture "day" Day :? "The date to retrieve commands, in ISO8601 format"
    :> "commands"
    :> Get '[JSON] [CommandView]

type GetFlow =
  Summary "Query flows"
    :> Capture "user" Text :? "User to query flows for"
    :> Capture "ref" Reference :? "A 'reference' expression denoting the event to retrieve"
    :> Get '[JSON] (Maybe Event)

type GetFlowsTimeline =
  Summary "Retrieve timeline of flows for a given day."
    :> Capture "user" Text :? "User name to query timeline for"
    :> Capture "day" Day :? "The date to retrieve flows timeline, in ISO8601 format"
    :> Get '[JSON] [FlowView]

type GetGroupedTimelines =
  Summary
    "Retrieve timeline of flows, grouped by some time slice (Day, Week, Month...). \
    \ If no 'group' param is given, returns _all_ flows."
    :> Capture "user" Text :? "User name to query timeline for"
    :> QueryParams "group" Group
      :? "The periods to group summary. There may be several of them \
         \ in which case the views will be grouped hierarchically (WIP)"
    :> Get '[JSON] [GroupViews FlowView]

type GetAllLog =
  Summary "Retrieve the complete log of all events pertaining to a given user, most recent first"
    :> Capture "user" Text :? "User to retrieve log of events for"
    :> QueryParam "page" Natural
      :? "Pagination parameter for retrieving events. Events are returned \
         \ 50 by 50, most recent event first. The 'next' and 'prev' pages \
         \ URLs are provided as 'Link' header"
    :> Get '[JSON] (Headers '[Header "Link" Text] [Event])

type GetFreshToken =
  Summary "Retrieve a fresh signed JWT token for given user."
    :> Capture "user" Text :? "The user to retrieve a token for"
    :> "token"
    :> Get '[JSON] SerializedToken

type GetUserProfile =
  Summary "Retrieve a user's profile."
    :> Capture "user" Text :? "The user to get profile of"
    :> Get '[JSON] UserProfile

type UpdateUserProfile =
  Summary "Update an existing user's profile. If the user does not exist, an error is thrown"
    :> Capture "user" Text :? "User to update profile of"
    :> ReqBody '[JSON] UserProfile
    :> Put '[JSON] NoContent

type CreateUserProfile =
  Summary
    "Create a new user, setting his or her initial profile. The name of \
    \ the user is inferred from the profile's content. \
    \ If the user already exists, returns a 400, otherwise, return the user's \
    \ unique identifier as an hex-encoded string."
    :> ReqBody '[JSON] UserProfile
    :> Post '[JSON] (Encoded Hex)

type LoginAPI =
  Summary "Allows users to login passing in credentials."
    :> Description
         "If successful, this will set cookies containing user's data in the form of JWT token.\
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
             :> (GetFreshToken :<|> CreateUserProfile :<|> GetUserProfile :<|> UpdateUserProfile)
           :<|> Tags "Metadata" :> DisplayVersions
       )

senseiAPI :: Proxy SenseiAPI
senseiAPI = Proxy
