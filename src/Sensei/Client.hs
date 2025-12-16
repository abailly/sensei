
module Sensei.Client (
  ClientMonad (..),
  ClientConfig (..),
  SenseiClientConfig (..),
  ClientError,
  defaultConfig,
  killC,
  loginC,
  postEventC,
  getFlowC,
  updateFlowC,
  queryFlowC,
  queryFlowDayC,
  queryFlowPeriodSummaryC,
  notesDayC,
  commandsDayC,
  searchNotesC,
  getLogC,
  getFreshTokenC,
  createUserProfileC,
  getUserProfileC,
  setUserProfileC,
  getVersionsC,
  postGoalC,
  getGoalsC,
  send,
) where

import Data.Functor (void)
import Data.Text (Text)
import Data.Time
import Preface.Codec (Encoded, Hex)
import Sensei.API
import Sensei.Client.Monad
import Sensei.Server.Auth (Credentials, SerializedToken)
import Sensei.Version
import Servant
import Servant.Client
import Servant.Client.Core

killC :: ClientMonad SenseiClientConfig ()
killC = clientIn (Proxy @KillServer) Proxy

loginC :: Credentials -> ClientMonad SenseiClientConfig ()
loginC = void . clientIn (Proxy @LoginAPI) Proxy

postEventC :: UserName -> [Event] -> ClientMonad SenseiClientConfig ()
getFlowC :: Text -> Reference -> ClientMonad SenseiClientConfig (Maybe EventView)
updateFlowC :: Text -> TimeDifference -> ClientMonad SenseiClientConfig Event
queryFlowC :: Text -> [Group] -> ClientMonad SenseiClientConfig [GroupViews FlowView]
queryFlowDayC :: Text -> Day -> ClientMonad SenseiClientConfig [FlowView]
queryFlowPeriodSummaryC :: Text -> Maybe Day -> Maybe Day -> Maybe Group -> ClientMonad SenseiClientConfig (Headers '[Header "Link" Text] FlowSummary)
notesDayC :: Text -> Day -> ClientMonad SenseiClientConfig (Headers '[Header "Link" Text] [NoteView])
commandsDayC :: Text -> Day -> ClientMonad SenseiClientConfig [CommandView]
searchNotesC :: Text -> Maybe Text -> ClientMonad SenseiClientConfig [NoteView]
getLogC :: Text -> Maybe Natural -> ClientMonad SenseiClientConfig (Headers '[Header "Link" Text] [EventView])
getFreshTokenC :: Text -> ClientMonad SenseiClientConfig SerializedToken
createUserProfileC :: UserProfile -> ClientMonad SenseiClientConfig (Encoded Hex)
getUserProfileC :: ClientMonad SenseiClientConfig UserProfile
setUserProfileC :: Text -> UserProfile -> ClientMonad SenseiClientConfig NoContent
getVersionsC :: ClientMonad SenseiClientConfig Versions
postGoalC :: Text -> GoalOp -> ClientMonad SenseiClientConfig CurrentGoals
getGoalsC :: Text -> ClientMonad SenseiClientConfig Goals
( getFlowC :<|> updateFlowC
    :<|> queryFlowPeriodSummaryC
    :<|> notesDayC
    :<|> commandsDayC
    :<|> queryFlowDayC
    :<|> queryFlowC
  )
  :<|> searchNotesC
  :<|> (postEventC :<|> getLogC)
  :<|> (getFreshTokenC :<|> createUserProfileC :<|> getUserProfileC :<|> setUserProfileC)
  :<|> getVersionsC
  :<|> (postGoalC :<|> getGoalsC) = clientIn (Proxy @SenseiAPI) Proxy
