{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.App where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Swagger (Swagger)
import Network.Wai.Application.Static
import Preface.Server
import Sensei.API
import Sensei.Server
import Sensei.Server.OpenApi
import Sensei.Version
import Servant
import System.Directory
import System.FilePath
import System.Posix.Daemonize

type FullAPI =
  "swagger.json" :> Get '[JSON] Swagger
    :<|> (KillServer :<|> (CheckVersion :> SenseiAPI))
    :<|> Raw

fullAPI :: Proxy FullAPI
fullAPI = Proxy

sensei :: FilePath -> IO ()
sensei output = do
  signal <- newEmptyMVar
  server <- startAppServer "" [] 23456 (pure $ senseiApp signal output)
  waitServer server `race_` (takeMVar signal >> stopServer server)

baseServer :: MVar () -> FilePath -> Server (KillServer :<|> SenseiAPI)
baseServer signal output =
  killS signal
    :<|> traceS output
    :<|> ( flowS output
             :<|> queryFlowSummaryS output
             :<|> queryFlowDaySummaryS output
             :<|> notesDayS output
             :<|> commandsDayS output
             :<|> queryFlowDayS output
             :<|> queryFlowS output
         )
    :<|> userProfileS

senseiApp :: MVar () -> FilePath -> Application
senseiApp signal output =
  serve fullAPI $
    pure senseiSwagger
      :<|> baseServer signal output
      :<|> Tagged staticResources

-- | Serve static resources under `public/` directory

staticResources :: Application
staticResources = staticApp (defaultFileServerSettings "ui")

senseiLog :: IO FilePath
senseiLog = (</> ".sensei.log") <$> getHomeDirectory

daemonizeServer :: IO ()
daemonizeServer =
  daemonize $
    -- TODO fix this silly hardcoded path
    -- this is so because I want to ensure the server is started in a location
    -- where it can find the FE resources...
    withCurrentDirectory "/Users/arnaud/projects/pankzsoft/sensei/" $ startServer

startServer :: IO ()
startServer =
  senseiLog >>= sensei
