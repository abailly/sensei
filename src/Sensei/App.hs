module Sensei.App where

import Network.Wai.Handler.Warp
import Sensei.Server
import Sensei.API
import Servant
import System.Directory
import System.FilePath
import System.Posix.Daemonize

sensei :: FilePath -> IO ()
sensei output =
  run 23456 (serve senseiAPI $ traceS output :<|> (flowS output :<|> queryFlowDaySummaryS output :<|> queryFlowDayS output :<|> queryFlowS output))

senseiLog :: IO FilePath
senseiLog = (</> ".sensei.log") <$> getHomeDirectory

daemonizeServer :: IO ()
daemonizeServer =
  daemonize startServer

startServer :: IO ()
startServer =
  senseiLog >>= sensei
