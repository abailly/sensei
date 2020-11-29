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
import Control.Exception.Safe(try)
import Data.Swagger (Swagger)
import Preface.Server
import Sensei.API
import Sensei.DB
import Sensei.DB.File
import Sensei.Server
import Sensei.Server.Config
import Sensei.Server.OpenApi
import Sensei.Server.UI
import Sensei.Version
import Servant
import System.Directory
import System.FilePath
import System.Posix.Daemonize
import System.Environment (lookupEnv, setEnv)

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
  env <- (>>= readEnv) <$> lookupEnv "ENVIRONMENT"
  server <- startAppServer "" [] 23456 (senseiApp env signal output configDir)
  waitServer server `race_` (takeMVar signal >> stopServer server)
  where
    getConfigDirectory = do
      home <- getXdgDirectory XdgConfig "sensei"
      exists <- doesDirectoryExist home
      when (not exists) $ createDirectory home
      pure home

baseServer ::
  (MonadIO m, DB m) =>
  MVar () ->
  ServerT (KillServer :<|> SenseiAPI) m
baseServer signal =
  killS signal
    :<|> traceS
    :<|> ( flowS
             :<|> queryFlowSummaryS
             :<|> queryFlowDaySummaryS
             :<|> notesDayS
             :<|> commandsDayS
             :<|> queryFlowDayS
             :<|> queryFlowS
         )
    :<|> (getUserProfileS :<|> putUserProfileS)
    :<|> getVersionsS

senseiApp :: Maybe Env -> MVar () -> FilePath -> FilePath -> IO Application
senseiApp env signal output configDir = do
  runFileDB output configDir $ initLogStorage
  pure $
    serve fullAPI $
      hoistServer fullAPI (Handler . ExceptT . try . runFileDB output configDir) $
        pure senseiSwagger
          :<|> baseServer signal
          :<|> Tagged (userInterface env)

senseiLog :: IO FilePath
senseiLog = (</> ".sensei.log") <$> getHomeDirectory

daemonizeServer :: IO ()
daemonizeServer = do
  setEnv "ENVIRONMENT" "Prod"
  daemonize startServer

startServer :: IO ()
startServer =
  senseiLog >>= sensei
