{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Sensei.DB (DB (initLogStorage, insertProfile, readProfile))
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
        void $ initDB rootUser rootPassword output configDir logger
        senseiApp env signal key output configDir logger
    )
    $ \server ->
      waitServer server `race_` (takeMVar signal >> stopServer server)

-- | Initialises the DB and returns the root user's id.
initDB :: Maybe Text -> Maybe (Encoded Base64, Encoded Base64) -> FilePath -> FilePath -> LoggerEnv -> IO (Encoded Hex)
initDB rootUser rootPassword output configDir logger =
  runDB output configDir logger $ do
    initLogStorage
    maybe (pure "") ensureUserExists rootUser
 where
  ensureUserExists userName = do
    prof <- try @_ @SQLiteDBError $ readProfile userName
    let rootProfile = defaultProfile{userName, userPassword = fromMaybe ("", "") rootPassword}
    either (const $ insertProfile rootProfile) (pure . userId) prof

senseiApp :: Maybe Env -> MVar () -> JWK -> FilePath -> FilePath -> LoggerEnv -> IO Application
senseiApp env signal publicAuthKey output configDir logger = do
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
          :<|> validateAuth jwtConfig cookieConfig
          :<|> Tagged (userInterface env)
 where
  runApp :: ReaderT LoggerEnv SQLiteDB x -> Handler x
  runApp = Handler . ExceptT . try . handleDBError . runDB output configDir logger . flip runReaderT logger

  handleDBError :: IO a -> IO a
  handleDBError io =
    io `catch` \(SQLiteDBError _q txt) -> throwM $ err500{errBody = fromStrict $ encodeUtf8 txt}

  validateAuth jwtConfig cookieConfig (Authenticated (AuthToken userId _)) =
    baseServer jwtConfig cookieConfig userId
  validateAuth _ _ _ = throwAll err401{errHeaders = [("www-authenticate", "Bearer realm=\"sensei\"")]}

  baseServer ::
    (MonadIO m, DB m) =>
    JWTSettings ->
    CookieSettings ->
    Encoded Hex ->
    ServerT (KillServer :<|> LogoutAPI :<|> SetCurrentTime :<|> GetCurrentTime :<|> SenseiAPI) m
  baseServer jwtSettings cookieSettings userId =
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
      :<|> (postEventS :<|> getLogS)
      :<|> (getFreshTokenS jwtSettings :<|> createUserProfileS :<|> getUserProfileIdS userId :<|> setUserProfileS)
      :<|> getVersionsS
      :<|> (postGoalS :<|> getGoalsS)

-- | This orphan instance is needed because of the 'validateAuth' function above
instance MonadError ServerError SQLiteDB where
  throwError = throwM
  catchError = catch
