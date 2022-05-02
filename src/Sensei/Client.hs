{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.Client (
    ClientMonad (..),
    ClientConfig (..),
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

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (throwIO)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
import Network.HTTP.Client (createCookieJar, defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import Network.URI.Extra (uriToString')
import Preface.Codec (Encoded, Hex)
import Sensei.API
import Sensei.App
import Sensei.Client.Monad
import Sensei.Server.Auth (Credentials, SerializedToken)
import Sensei.Version
import Servant
import Servant.Client
import Servant.Client.Core

killC :: ClientMonad ()
killC = clientIn (Proxy @KillServer) Proxy

loginC :: Credentials -> ClientMonad ()
loginC = void . clientIn (Proxy @LoginAPI) Proxy

postEventC :: UserName -> [Event] -> ClientMonad ()
getFlowC :: Text -> Reference -> ClientMonad (Maybe EventView)
updateFlowC :: Text -> TimeDifference -> ClientMonad Event
queryFlowC :: Text -> [Group] -> ClientMonad [GroupViews FlowView]
queryFlowDayC :: Text -> Day -> ClientMonad [FlowView]
queryFlowPeriodSummaryC :: Text -> Maybe Day -> Maybe Day -> Maybe Group -> ClientMonad (Headers '[Header "Link" Text] FlowSummary)
notesDayC :: Text -> Day -> ClientMonad (Headers '[Header "Link" Text] [NoteView])
commandsDayC :: Text -> Day -> ClientMonad [CommandView]
searchNotesC :: Text -> Maybe Text -> ClientMonad [NoteView]
getLogC :: Text -> Maybe Natural -> ClientMonad (Headers '[Header "Link" Text] [EventView])
getFreshTokenC :: Text -> ClientMonad SerializedToken
createUserProfileC :: UserProfile -> ClientMonad (Encoded Hex)
getUserProfileC :: Text -> ClientMonad UserProfile
setUserProfileC :: Text -> UserProfile -> ClientMonad NoContent
getVersionsC :: ClientMonad Versions
postGoalC :: Text -> GoalOp -> ClientMonad CurrentGoals
getGoalsC :: Text -> ClientMonad Goals
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

send :: ClientConfig -> ClientMonad a -> IO a
send config@ClientConfig{serverUri, startServerLocally} act = do
    let base = fromMaybe (BaseUrl Http "localhost" 23456 "") $ parseBaseUrl $ uriToString' serverUri
    mgr <- case baseUrlScheme base of
        Http -> newManager defaultManagerSettings
        Https -> newManager tlsManagerSettings
    jar <- newTVarIO (createCookieJar [])
    let env = (mkClientEnv mgr base){cookieJar = Just jar}
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
