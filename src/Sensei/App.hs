{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.App where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad.Except
import Control.Monad.Reader
import Data.Swagger (Swagger)
import Network.Wai.Application.Static
import Preface.Server
import Sensei.API
import Sensei.IO
import Sensei.Server
import Sensei.Server.OpenApi
import Sensei.Version
import Servant
import System.Directory
import System.FilePath
import System.Posix.Daemonize

type FullAPI =
  "swagger.json" :> Get '[JSON] Swagger
    :<|> (KillServer :<|> (CheckVersion $(senseiVersionTH) :> SenseiAPI))
    :<|> Raw

fullAPI :: Proxy FullAPI
fullAPI = Proxy

sensei :: FilePath -> IO ()
sensei output = do
  signal <- newEmptyMVar
  configDir <- getConfigDirectory
  server <- startAppServer "" [] 23456 (senseiApp signal output configDir)
  waitServer server `race_` (takeMVar signal >> stopServer server)
  where
    getConfigDirectory = do
      home <- getXdgDirectory XdgConfig "sensei"
      exists <- doesDirectoryExist home
      when (not exists) $ createDirectory home
      pure home

baseServer ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) =>
  MVar () ->
  FilePath ->
  ServerT (KillServer :<|> SenseiAPI) m
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
    :<|> (getUserProfileS
          :<|> putUserProfileS)
    :<|> getVersionsS

senseiApp :: MVar () -> FilePath -> FilePath -> IO Application
senseiApp signal output configDir = do
  initLogStorage output
  pure $
    serve fullAPI $
      hoistServer fullAPI (`runReaderT` configDir) $
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
