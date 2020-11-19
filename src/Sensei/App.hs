{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Reader
import Control.Monad.Except

type FullAPI =
  "swagger.json" :> Get '[JSON] Swagger
    :<|> (KillServer :<|> (CheckVersion :> SenseiAPI))
    :<|> Raw

fullAPI :: Proxy FullAPI
fullAPI = Proxy

sensei :: FilePath -> IO ()
sensei output = do
  signal <- newEmptyMVar
  configDir <- getConfigDirectory
  server <- startAppServer "" [] 23456 (pure $ senseiApp signal output configDir)
  waitServer server `race_` (takeMVar signal >> stopServer server)
  where
    getConfigDirectory = do
      home <- getXdgDirectory XdgConfig "sensei"
      exists <- doesDirectoryExist home
      when (not exists) $ createDirectory home
      pure home

baseServer ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) =>
  MVar () -> FilePath ->  ServerT (KillServer :<|> SenseiAPI) m
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
    :<|> getUserProfileS
    :<|> putUserProfileS

senseiApp :: MVar () -> FilePath -> FilePath -> Application
senseiApp signal output configDir =
  serve fullAPI $ hoistServer fullAPI (`runReaderT` configDir) $
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
