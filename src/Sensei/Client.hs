{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.Client where

import Control.Concurrent (threadDelay)
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

postEventC :: Event -> ClientMonad ()
getFlowC :: Text -> Reference -> ClientMonad (Maybe Event)
updateFlowC :: Text -> TimeDifference -> ClientMonad Event
queryFlowC :: Text -> [Group] -> ClientMonad [GroupViews FlowView]
queryFlowSummaryC :: Text -> ClientMonad [GroupViews (FlowType, NominalDiffTime)]
queryFlowDayC :: Text -> Day -> ClientMonad [FlowView]
queryFlowDaySummaryC :: Text -> Day -> ClientMonad FlowSummary
notesDayC :: Text -> Day -> ClientMonad (Headers '[Header "Link" Text] [NoteView])
commandsDayC :: Text -> Day -> ClientMonad [CommandView]
searchNotesC :: Text -> Maybe Text -> ClientMonad [NoteView]
getLogC :: Text -> Maybe Natural -> ClientMonad (Headers '[Header "Link" Text] [Event])
getUserProfileC :: Text -> ClientMonad UserProfile
setUserProfileC :: Text -> UserProfile -> ClientMonad NoContent
getVersionsC :: ClientMonad Versions
( getFlowC :<|> updateFlowC :<|> queryFlowSummaryC
           :<|> queryFlowDaySummaryC
           :<|> notesDayC
           :<|> commandsDayC
           :<|> queryFlowDayC
           :<|> queryFlowC
         )
  :<|> searchNotesC
  :<|> (postEventC :<|> getLogC)
  :<|> (getUserProfileC :<|> setUserProfileC)
  :<|> getVersionsC = clientIn senseiAPI Proxy

newtype ClientMonad a = ClientMonad {unClient :: forall m. (RunClient m) => m a}

instance Functor ClientMonad where
  fmap f (ClientMonad a) = ClientMonad $ fmap f a

instance Applicative ClientMonad where
  pure a = ClientMonad (pure a)
  ClientMonad f <*> ClientMonad a = ClientMonad $ f <*> a

instance Monad ClientMonad where
  ClientMonad a >>= f =
    ClientMonad $ a >>= unClient . f

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

  throwClientError err = ClientMonad $ throwClientError err

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
