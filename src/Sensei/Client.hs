{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Sensei.Client where

import Data.CaseInsensitive
import Data.Time
import Control.Monad.Trans
import Data.Text(Text, pack)
import Data.Text.Encoding(encodeUtf8)
import Data.Sequence
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Sensei.API
import Servant
import Servant.Client
import Servant.Client.Core

import Sensei.App

traceC :: Trace -> ClientMonad ()
flowC :: Text -> FlowType -> FlowState -> ClientMonad ()
queryFlowC :: Text -> [Group] -> ClientMonad [GroupViews FlowView]
queryFlowSummaryC :: Text -> ClientMonad [GroupViews (FlowType, NominalDiffTime)]
queryFlowDayC :: Text -> Day -> ClientMonad [FlowView]
queryFlowDaySummaryC :: Text -> Day -> ClientMonad [(FlowType, NominalDiffTime)]
notesDayC :: Text -> Day -> ClientMonad [(LocalTime, Text)]
traceC :<|> (flowC :<|> queryFlowSummaryC :<|> queryFlowDaySummaryC :<|> notesDayC :<|> queryFlowDayC:<|> queryFlowC) :<|> _ = clientIn senseiAPI Proxy

newtype ClientMonad a = ClientMonad { unClient :: ClientM a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance RunClient ClientMonad where
  runRequest req = ClientMonad $ do
    let request = req { requestHeaders =  (mk "Host", encodeUtf8 $ pack $ "localhost:23456") <|
                                          (mk "Origin", "http://localhost:23456") <|
                                          requestHeaders req  }
    runRequest request

  throwClientError = ClientMonad . throwClientError

send :: ClientMonad a -> IO a
send (ClientMonad act) = do
  mgr <- newManager defaultManagerSettings
  let base = BaseUrl Http "127.0.0.1" 23456 ""
  res <- runClientM act (ClientEnv mgr base Nothing)
  case res of
    -- server is not running, fork it
    Left (ConnectionError _) -> do
      daemonizeServer
      -- retry sending the trace to server
      send (ClientMonad act)
    -- something is wrong, bail out
    Left otherError -> error $ "failed to connect to server: " <> show otherError
    Right v -> pure v
