{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import Network.URI.Extra (uriToString')
import Sensei.API
import Sensei.App
import Sensei.Client.Monad
import Sensei.Version
import Servant
import Servant.Client
import Servant.Client.Core

killC :: ClientMonad ()
killC = clientIn (Proxy @KillServer) Proxy

postEventC :: [Event] -> ClientMonad ()
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
send config@ClientConfig {serverUri, startServerLocally} act = do
  let base = fromMaybe (BaseUrl Http "localhost" 23456 "") $ parseBaseUrl $ uriToString' serverUri
  mgr <- case baseUrlScheme base of
    Http -> newManager defaultManagerSettings
    Https -> newManager tlsManagerSettings
  let env = mkClientEnv mgr base
  res <- runClientM (runReaderT (unClient act) config) env
  case res of
    Left err ->
      if startServerLocally
        then handleError env err
        else throwIO err
    Right v -> pure v
  where
    handleError env = \case
      -- server is not running, fork it
      -- TODO: probably not such a good idea?
      ConnectionError _ -> daemonizeServer >> error "Should never happen" -- daemonizeserver exits the current process

      -- incorrect version, kill server and retry
      -- TODO: user 'Accept: ' header with proper mime-type instead of custome
      -- header hijacking 406 response code
      FailureResponse _req resp
        | responseStatusCode resp == notAcceptable406 -> do
          -- we ignore the result of kill as it probaly will be an error: the server
          -- might not stop gracefully, in time to return us a response so yolo and
          -- simply give it some time to restart...
          runClientM (runReaderT (unClient killC) config) env >> threadDelay 1000000
          send config act

      -- something is wrong, bail out
      otherError -> throwIO otherError
