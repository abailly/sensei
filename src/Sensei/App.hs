{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.App where

import Data.Swagger
import Network.Wai.Application.Static
import Sensei.API
import Sensei.Server
import Sensei.Server.OpenApi
import Servant
import System.Directory
import System.FilePath
import System.Posix.Daemonize
import Preface.Server

type FullAPI =
  "swagger.json" :> Get '[JSON] Swagger
    :<|> SenseiAPI

fullAPI :: Proxy FullAPI
fullAPI = Proxy

sensei :: FilePath -> IO ()
sensei output =
  startAppServer "" [] 23456 (pure $ senseiApp output) >>= waitServer

senseiApp :: FilePath -> Application
senseiApp output =
  serve fullAPI $
    pure senseiSwagger
      :<|> traceS output
      :<|> ( flowS output
               :<|> queryFlowSummaryS output
               :<|> queryFlowDaySummaryS output
               :<|> notesDayS output
               :<|> queryFlowDayS output
               :<|> queryFlowS output
           )
      :<|> userProfileS
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
