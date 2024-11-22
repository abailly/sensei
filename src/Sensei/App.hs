{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.App where

import Control.Concurrent.Async (race_)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Exception.Safe (catch, throwM, try)
import Control.Monad (void)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (decodeStrict, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Swagger (Swagger)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.CORS (WithCORS (..))
import Preface.Codec (Base64, Encoded, Hex)
import Preface.Log (LoggerEnv)
import Preface.Server (
  AppServerConfig (
    AppServerConfig,
    cors,
    listenPort,
    serverAssignedName
  ),
  stopServer,
  waitServer,
  withAppServer,
 )
import Sensei.API (
  GetCurrentTime,
  KillServer,
  LoginAPI,
  LogoutAPI,
  Protected,
  SenseiAPI,
  SetCurrentTime,
  UserProfile (userId, userName, userPassword),
  defaultProfile,
 )
import Sensei.Backend.Class (Backends)
import Sensei.DB (DB (initLogStorage, insertProfile, readProfile), DBError)
import Sensei.DB.Log ()
import Sensei.DB.SQLite (
  SQLiteDB,
  SQLiteDBError (..),
  getDataFile,
  runDB,
 )
import Sensei.Server
import Sensei.Version (CheckVersion, senseiVersionTH)
import Servant (
  Application,
  Context (EmptyContext, (:.)),
  Get,
  Handler (Handler),
  HasServer (ServerT, hoistServerWithContext),
  JSON,
  Proxy (..),
  Raw,
  ServerError (errBody, errHeaders),
  Tagged (Tagged),
  err401,
  err500,
  serveWithContext,
  type (:<|>) (..),
  type (:>),
 )
import System.Environment (lookupEnv)
import System.FilePath ((</>))

type AppM db = ReaderT LoggerEnv db

type FullAPI =
  "swagger.json" :> Get '[JSON] Swagger
    :<|> LoginAPI
    :<|> Protected
      :> ( KillServer
            :<|> LogoutAPI
            :<|> SetCurrentTime
            :<|> GetCurrentTime
            :<|> (CheckVersion $senseiVersionTH :> SenseiAPI)
         )
    :<|> Raw

fullAPI :: Proxy FullAPI
fullAPI = Proxy

getKeyAsString :: FilePath -> IO String
getKeyAsString configDir = do
  let keyFile = configDir </> "sensei.jwk"
  key <-
    getKey keyFile
      `catch` ( \(_ :: IOError) -> do
                  k <- makeNewKey
                  LBS.writeFile keyFile $ encode k
                  pure k
              )
  pure $ unpack . decodeUtf8 . toStrict . encode $ key

startServer :: FilePath -> IO ()
startServer configDir =
  getDataFile configDir >>= sensei

sensei :: FilePath -> IO ()
sensei output = do
  signal <- newEmptyMVar
  configDir <- fromMaybe "." <$> lookupEnv "SENSEI_SERVER_CONFIG_DIR"
  key <- readOrMakeKey =<< lookupEnv "SENSEI_SERVER_KEY"
  serverName <- pack . fromMaybe "" <$> lookupEnv "SENSEI_SERVER_NAME"
  serverPort <- readPort <$> lookupEnv "SENSEI_SERVER_PORT"
  rootUser <- fmap pack <$> lookupEnv "SENSEI_SERVER_ROOT_USER"
  rootPassword <- (>>= decodeStrict . encodeUtf8 . Text.pack) <$> lookupEnv "SENSEI_SERVER_ROOT_PASSWORD"
  env <- (>>= readEnv) <$> lookupEnv "ENVIRONMENT"
  let serverConfig =
        AppServerConfig
          { serverAssignedName = serverName
          , cors = NoCORS
          , listenPort = serverPort
          }
  withAppServer
    serverConfig
    ( \logger -> do
        let dbRunner = runDB output configDir logger

            handleDBError io =
              io `catch` \(SQLiteDBError _q txt) -> throwM $ err500{errBody = fromStrict $ encodeUtf8 txt}

            runApp = Handler . ExceptT . try . handleDBError . dbRunner . flip runReaderT logger

        void $ initDB rootUser rootPassword dbRunner
        senseiApp env signal key runApp undefined
    )
    $ \server ->
      waitServer server `race_` (takeMVar signal >> stopServer server)

-- | Initialises the DB and returns the root user's id.
initDB :: forall db. DB db => Maybe Text -> Maybe (Encoded Base64, Encoded Base64) -> (forall x. db x -> IO x) -> IO (Encoded Hex)
initDB rootUser rootPassword dbRunner =
  dbRunner $ do
    initLogStorage
    maybe (pure "") ensureUserExists rootUser
 where
  ensureUserExists :: Text -> db (Encoded Hex)
  ensureUserExists userName = do
    prof <- try @_ @(DBError db) $ readProfile userName
    let rootProfile = defaultProfile{userName, userPassword = fromMaybe ("", "") rootPassword}
    either (const $ insertProfile rootProfile) (pure . userId) prof

senseiApp ::
  (MonadIO db, DB db, MonadError ServerError db) =>
  Maybe Env ->
  MVar () ->
  JWK ->
  (forall x. AppM db x -> Handler x) ->
  Backends ->
  IO Application
senseiApp env signal publicAuthKey runApp backends = do
  let jwtConfig = defaultJWTSettings publicAuthKey
      cookieConfig = defaultCookieSettings{cookieXsrfSetting = Nothing}
      contextConfig = jwtConfig :. cookieConfig :. EmptyContext
      contextProxy :: Proxy [JWTSettings, CookieSettings]
      contextProxy = Proxy
  pure $
    serveWithContext fullAPI contextConfig $
      hoistServerWithContext fullAPI contextProxy runApp $
        pure senseiSwagger
          :<|> loginS jwtConfig cookieConfig
          :<|> validateAuth backends jwtConfig cookieConfig
          :<|> Tagged (userInterface env)
 where
  validateAuth backendMap jwtConfig cookieConfig (Authenticated (AuthToken userId _)) =
    baseServer backendMap jwtConfig cookieConfig userId
  validateAuth _ _ _ _ = throwAll err401{errHeaders = [("www-authenticate", "Bearer realm=\"sensei\"")]}

  baseServer ::
    (MonadIO m, DB m) =>
    Backends ->
    JWTSettings ->
    CookieSettings ->
    Encoded Hex ->
    ServerT (KillServer :<|> LogoutAPI :<|> SetCurrentTime :<|> GetCurrentTime :<|> SenseiAPI) m
  baseServer backendMap jwtSettings cookieSettings userId =
    killS signal
      :<|> logoutS cookieSettings
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
      :<|> (postEventS backendMap :<|> getLogS)
      :<|> (getFreshTokenS jwtSettings :<|> createUserProfileS :<|> getUserProfileIdS userId :<|> setUserProfileS)
      :<|> getVersionsS
      :<|> (postGoalS :<|> getGoalsS)

-- | This orphan instance is needed because of the 'validateAuth' function above
instance MonadError ServerError SQLiteDB where
  throwError = throwM
  catchError = catch
