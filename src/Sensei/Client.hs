{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.Client
  ( ClientMonad (..),
    ClientConfig (..),
    ClientError,
    killC,
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
    getUserProfileC,
    setUserProfileC,
    getVersionsC,
    send,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Time
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status
import Sensei.API
import Sensei.App
import Sensei.Client.Monad
import Sensei.Version
import Servant
import Servant.Client
import Servant.Client.Core

killC :: ClientMonad ()
killC = clientIn (Proxy @KillServer) Proxy

postEventC :: Event -> ClientMonad ()
getFlowC :: Text -> Reference -> ClientMonad (Maybe Event)
updateFlowC :: Text -> TimeDifference -> ClientMonad Event
queryFlowC :: Text -> [Group] -> ClientMonad [GroupViews FlowView]
queryFlowDayC :: Text -> Day -> ClientMonad [FlowView]
queryFlowPeriodSummaryC :: Text -> Maybe Day -> Maybe Day -> Maybe Group -> ClientMonad (Headers '[Header "Link" Text] FlowSummary)
notesDayC :: Text -> Day -> ClientMonad (Headers '[Header "Link" Text] [NoteView])
commandsDayC :: Text -> Day -> ClientMonad [CommandView]
searchNotesC :: Text -> Maybe Text -> ClientMonad [NoteView]
getLogC :: Text -> Maybe Natural -> ClientMonad (Headers '[Header "Link" Text] [Event])
getUserProfileC :: Text -> ClientMonad UserProfile
setUserProfileC :: Text -> UserProfile -> ClientMonad NoContent
getVersionsC :: ClientMonad Versions
( getFlowC :<|> updateFlowC
    :<|> queryFlowPeriodSummaryC
    :<|> notesDayC
    :<|> commandsDayC
    :<|> queryFlowDayC
    :<|> queryFlowC
  )
  :<|> searchNotesC
  :<|> (postEventC :<|> getLogC)
  :<|> (getUserProfileC :<|> setUserProfileC)
  :<|> getVersionsC = clientIn (Proxy @SenseiAPI) Proxy

send :: ClientConfig -> ClientMonad a -> IO a
send config@ClientConfig {serverHost, serverPort} act = do
  mgr <- newManager defaultManagerSettings
  let base = BaseUrl Http serverHost serverPort ""
      env = mkClientEnv mgr base
  res <- runClientM (runReaderT (unClient act) config) env
  case res of
    -- server is not running, fork it
    -- TODO: probably not such a good idea?
    Left (ConnectionError _) -> daemonizeServer >> error "Should never happen" -- daemonizeserver exits the current process

    -- incorrect version, kill server and retry
    -- TODO: user 'Accept: ' header with proper mime-type instead of custome
    -- header hijacking 406 response code
    Left (FailureResponse _req resp)
      | responseStatusCode resp == notAcceptable406 -> do
        -- we ignore the result of kill as it probaly will be an error: the server
        -- might not stop gracefully, in time to return us a response so yolo and
        -- simply give it some time to restart...
        runClientM (runReaderT (unClient killC) config) env >> threadDelay 1000000
        send config act

    -- something is wrong, bail out
    Left otherError -> throwIO otherError
    -- everything's right
    Right v -> pure v
