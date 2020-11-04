module Sensei.Client where

import Data.Time
import Data.Text(Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Sensei.API
import Servant
import Servant.Client

import Sensei.App

traceC :: Trace -> ClientM ()
flowC :: Text -> FlowType -> FlowState -> ClientM ()
queryFlowC :: Text -> [Group] -> ClientM [GroupViews FlowView]
queryFlowSummaryC :: Text -> ClientM [GroupViews (FlowType, NominalDiffTime)]
queryFlowDayC :: Text -> Day -> ClientM [FlowView]
queryFlowDaySummaryC :: Text -> Day -> ClientM [(FlowType, NominalDiffTime)]
notesDayC :: Text -> Day -> ClientM [(LocalTime, Text)]
traceC :<|> (flowC :<|> queryFlowSummaryC :<|> queryFlowDaySummaryC :<|> notesDayC :<|> queryFlowDayC:<|> queryFlowC) :<|> _ = client senseiAPI

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
