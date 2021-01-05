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
import Control.Exception.Safe (throwM, try, catch)
import Control.Monad.Except
import Data.Text(pack)
import Data.ByteString.Lazy(fromStrict)
import Data.Text.Encoding(encodeUtf8)
import Data.Swagger (Swagger)
import Preface.Server
import Preface.Log
import Sensei.API
import Sensei.DB
import Sensei.DB.SQLite
import Sensei.DB.Log()
import Sensei.IO
import Sensei.Server
import Sensei.Server.Config
import Sensei.Server.OpenApi
import Sensei.Server.UI
import Sensei.Version
import Servant
import System.Environment (lookupEnv, setEnv)
import System.Posix.Daemonize
import Control.Monad.Reader (ReaderT(runReaderT))
import Data.Maybe (fromMaybe)

type FullAPI =
  "swagger.json" :> Get '[JSON] Swagger
    :<|> (KillServer :<|> SetCurrentTime :<|> GetCurrentTime :<|> (CheckVersion $(senseiVersionTH) :> SenseiAPI))
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
  serverName <- pack . fromMaybe "" <$> lookupEnv "SENSEI_SERVER_NAME"
  serverPort <- readPort <$> lookupEnv "SENSEI_SERVER_PORT"
  env <- (>>= readEnv) <$> lookupEnv "ENVIRONMENT"
  server <- startAppServer serverName [] serverPort (senseiApp env signal output configDir)
  waitServer server `race_` (takeMVar signal >> stopServer server)

readPort :: Maybe String -> Int
readPort Nothing = 23456
readPort (Just portString) =
  case reads portString of
    (p,[]):_ -> p
    _ -> error ("invalid environment variable SENSEI_SERVER_PORT "<> portString)


baseServer ::
  (MonadIO m, DB m) =>
  MVar () ->
  ServerT (KillServer :<|> SetCurrentTime :<|> GetCurrentTime :<|> SenseiAPI) m
baseServer signal =
  killS signal
    :<|> setCurrentTimeS
    :<|> getCurrentTimeS
    :<|> ( getFlowS
             :<|> updateFlowStartTimeS
             :<|> queryFlowSummaryS
             :<|> queryFlowDaySummaryS
             :<|> notesDayS
             :<|> commandsDayS
             :<|> queryFlowDayS
             :<|> queryFlowS
         )
    :<|> searchNoteS
    :<|> (postEventS :<|> getLogS)
    :<|> (getUserProfileS :<|> putUserProfileS)
    :<|> getVersionsS

senseiApp :: Maybe Env -> MVar () -> FilePath -> FilePath -> LoggerEnv -> IO Application
senseiApp env signal output configDir logger = do
  runDB output configDir logger $ initLogStorage
  pure $
    serve fullAPI $
      hoistServer fullAPI runApp $
        pure senseiSwagger
          :<|> baseServer signal
          :<|> Tagged (userInterface env)
  where
    runApp :: ReaderT LoggerEnv SQLiteDB x -> Handler x
    runApp = (Handler . ExceptT . try . handleDBError . runDB output configDir logger . flip runReaderT logger)

    handleDBError :: IO a -> IO a
    handleDBError io =
      io `catch` \ (SQLiteDBError _q txt) -> throwM $ err500 { errBody = fromStrict $ encodeUtf8 txt }
