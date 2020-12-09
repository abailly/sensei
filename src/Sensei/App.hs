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
import Control.Exception.Safe (try)
import Control.Monad.Except
import Data.Swagger (Swagger)
import Preface.Server
import Sensei.API
import Sensei.DB
import Sensei.DB.SQLite
import Sensei.IO
import Sensei.Server
import Sensei.Server.Config
import Sensei.Server.OpenApi
import Sensei.Server.UI
import Sensei.Version
import Servant
import System.Environment (lookupEnv, setEnv)
import System.Posix.Daemonize

type FullAPI =
  "swagger.json" :> Get '[JSON] Swagger
    :<|> (KillServer :<|> (CheckVersion $(senseiVersionTH) :> SenseiAPI))
    :<|> Raw

fullAPI :: Proxy FullAPI
fullAPI = Proxy

daemonizeServer :: IO ()
daemonizeServer = do
  setEnv "ENVIRONMENT" "Prod"
  daemonize startServer

startServer :: IO ()
startServer =
  getConfigDirectory >>= getDataFile >>= sensei

sensei :: FilePath -> IO ()
sensei output = do
  signal <- newEmptyMVar
  configDir <- getConfigDirectory
  env <- (>>= readEnv) <$> lookupEnv "ENVIRONMENT"
  server <- startAppServer "" [] 23456 (senseiApp env signal output configDir)
  waitServer server `race_` (takeMVar signal >> stopServer server)

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
  runDB output configDir $ initLogStorage
  pure $
    serve fullAPI $
      hoistServer fullAPI (Handler . ExceptT . try . runDB output configDir) $
        pure senseiSwagger
          :<|> baseServer signal
          :<|> Tagged (userInterface env)
