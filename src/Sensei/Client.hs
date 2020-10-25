module Sensei.Client where

import Data.Time
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Sensei.API
import Servant
import Servant.Client

import Sensei.App

traceC :: Trace -> ClientM ()
flowC :: FlowType -> FlowState -> ClientM ()
queryFlowC :: String -> ClientM [FlowView]
queryFlowDayC :: String -> Day -> ClientM [FlowView]
queryFlowDaySummaryC :: String -> Day -> ClientM [(FlowType, NominalDiffTime)]
traceC :<|> (flowC :<|> queryFlowDaySummaryC :<|> queryFlowDayC :<|> queryFlowC) = client senseiAPI

send :: ClientM a -> IO a
send act = do
  mgr <- newManager defaultManagerSettings
  let base = BaseUrl Http "127.0.0.1" 23456 ""
  res <- runClientM act (ClientEnv mgr base Nothing)
  case res of
    -- server is not running, fork it
    Left (ConnectionError _) -> do
      daemonizeServer
      -- retry sending the trace to server
      send act
    -- something is wrong, bail out
    Left otherError -> error $ "failed to connect to server: " <> show otherError
    Right v -> pure v
