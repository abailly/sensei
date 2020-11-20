{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.Client where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans
import Data.CaseInsensitive
import Data.Sequence
import Data.Text (Text)
import Data.Time
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status
import Sensei.API
import Sensei.App
import Sensei.Version
import Servant
import Servant.Client
import Servant.Client.Core

killC :: ClientMonad ()
killC = clientIn (Proxy @KillServer) Proxy

traceC :: Trace -> ClientMonad ()
flowC :: Text -> FlowType -> FlowState -> ClientMonad ()
queryFlowC :: Text -> [Group] -> ClientMonad [GroupViews FlowView]
queryFlowSummaryC :: Text -> ClientMonad [GroupViews (FlowType, NominalDiffTime)]
queryFlowDayC :: Text -> Day -> ClientMonad [FlowView]
queryFlowDaySummaryC :: Text -> Day -> ClientMonad [(FlowType, NominalDiffTime)]
notesDayC :: Text -> Day -> ClientMonad [NoteView]
commandsDayC :: Text -> Day -> ClientMonad [CommandView]

getUserProfileC :: Text -> ClientMonad UserProfile

traceC
  :<|> (flowC :<|> queryFlowSummaryC :<|> queryFlowDaySummaryC :<|> notesDayC :<|> commandsDayC :<|> queryFlowDayC :<|> queryFlowC)
  :<|> (getUserProfileC :<|> _) = clientIn senseiAPI Proxy

newtype ClientMonad a = ClientMonad {unClient :: ClientM a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance RunClient ClientMonad where
  runRequest req = ClientMonad $ do
    let request =
          req
            { requestHeaders =
                (mk "Host", "localhost:23456")
                  <| (mk "Origin", "http://localhost:23456")
                  <| (mk "X-API-Version", toHeader senseiVersion)
                  <| requestHeaders req
            }
    runRequest request

  throwClientError = ClientMonad . throwClientError

tryKillingServer :: ClientEnv -> IO ()
tryKillingServer env = do
  _ <- runClientM (unClient killC) env
  -- we ignore the result of kill as it probaly will be an error: the server
  -- might not stop gracefully, in time to return us a response so yolo and
  -- simply give it some time to restart...
  threadDelay 1000000

send :: ClientMonad a -> IO a
send act = do
  mgr <- newManager defaultManagerSettings
  let base = BaseUrl Http "127.0.0.1" 23456 ""
      env = ClientEnv mgr base Nothing
  res <- runClientM (unClient act) env
  case res of
    -- server is not running, fork it
    Left (ConnectionError _) -> do
      daemonizeServer
      -- retry sending the trace to server
      send act

    -- incorrect version, kill server and retry
    -- TODO: user 'Accept: ' header with proper mime-type instead of custome
    -- header hijacking 406 response code
    Left (FailureResponse _req resp)
      | responseStatusCode resp == notAcceptable406 -> do
        tryKillingServer env
        send act

    -- something is wrong, bail out
    Left otherError -> error $ "failed to connect to server: " <> show otherError
    -- everything's right
    Right v -> pure v
