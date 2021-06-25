{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.App where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception.Safe (catch, throwM, try)
import Control.Monad.Except
import Control.Monad.Reader (ReaderT (runReaderT))
import Crypto.JOSE (JWK)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromMaybe)
import Data.Swagger (Swagger)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Network.CORS (WithCORS (..))
import Preface.Log
import Preface.Server
import Sensei.API
import Sensei.DB
import Sensei.DB.Log ()
import Sensei.DB.SQLite
import Sensei.IO
import Sensei.Server
import Sensei.Server.Auth.API (Protected)
import Sensei.Server.Auth.Types
  ( AuthResult (..),
    CookieSettings,
    JWTSettings,
    defaultCookieSettings,
    defaultJWTSettings,
    getKey,
    throwAll,
    Credentials
  )
import Sensei.Server.Config
import Sensei.Server.OpenApi
import Sensei.Server.UI
import Sensei.Version
import Servant
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>))
import System.Posix.Daemonize

type FullAPI =
  "swagger.json" :> Get '[JSON] Swagger
    :<|> "login" :> ReqBody '[JSON]  Credentials :> Post '[JSON] NoContent
    :<|> Protected :> (KillServer :<|> SetCurrentTime :<|> GetCurrentTime :<|> (CheckVersion $(senseiVersionTH) :> SenseiAPI))
    :<|> Raw

fullAPI :: Proxy FullAPI
fullAPI = Proxy
  
daemonizeServer :: IO ()
daemonizeServer = do
  setEnv "ENVIRONMENT" "Prod"
  config <- readServerConfig =<< getConfigDirectory
  maybe raiseError (daemonize . startServer) config
  where
    raiseError = error $ "Server configuration file does not exist, not starting sensei daemon"

startServer :: ServerConfig -> IO ()
startServer serverConfig =
  getConfigDirectory >>= getDataFile >>= sensei serverConfig

sensei :: ServerConfig -> FilePath -> IO ()
sensei _serverConfig output = do
  signal <- newEmptyMVar
  configDir <- getConfigDirectory
  key <- getKey (configDir </> "sensei.jwk")
  serverName <- pack . fromMaybe "" <$> lookupEnv "SENSEI_SERVER_NAME"
  serverPort <- readPort <$> lookupEnv "SENSEI_SERVER_PORT"
  env <- (>>= readEnv) <$> lookupEnv "ENVIRONMENT"
  server <- startAppServer serverName NoCORS serverPort (senseiApp env signal key output configDir)
  waitServer server `race_` (takeMVar signal >> stopServer server)

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
             :<|> queryFlowPeriodSummaryS
             :<|> notesDayS
             :<|> commandsDayS
             :<|> queryFlowDayS
             :<|> queryFlowS
         )
    :<|> searchNoteS
    :<|> (postEventS :<|> getLogS)
    :<|> (getUserProfileS :<|> putUserProfileS)
    :<|> getVersionsS

senseiApp :: Maybe Env -> MVar () -> JWK -> FilePath -> FilePath -> LoggerEnv -> IO Application
senseiApp env signal publicAuthKey output configDir logger = do
  runDB output configDir logger $ initLogStorage
  let jwtConfig = defaultJWTSettings publicAuthKey
      cookieConfig = defaultCookieSettings
      contextConfig = jwtConfig :. cookieConfig :. EmptyContext
      contextProxy :: Proxy [JWTSettings, CookieSettings]
      contextProxy = Proxy
  pure $
    serveWithContext fullAPI contextConfig $
      hoistServerWithContext fullAPI contextProxy runApp $
        pure senseiSwagger
          :<|> loginS
          :<|> validateAuth
          :<|> Tagged (userInterface env)
  where
    validateAuth (Authenticated _) = baseServer signal
    validateAuth _ = throwAll err401 {errHeaders = [("www-authenticate", "Bearer realm=\"sensei\"")]}

    runApp :: ReaderT LoggerEnv SQLiteDB x -> Handler x
    runApp = (Handler . ExceptT . try . handleDBError . runDB output configDir logger . flip runReaderT logger)

    handleDBError :: IO a -> IO a
    handleDBError io =
      io `catch` \(SQLiteDBError _q txt) -> throwM $ err500 {errBody = fromStrict $ encodeUtf8 txt}

-- | This orphan instance is needed because of the 'validateAuth' function above
instance MonadError ServerError SQLiteDB where
  throwError = throwM
  catchError = catch
