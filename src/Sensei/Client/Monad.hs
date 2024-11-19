{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Sensei.Client.Monad (
  ClientConfig (..),
  SenseiClientConfig (..),
  ClientMonad (..),
  module Control.Monad.Reader,
  defaultConfig,
) where

import Control.Applicative ((<|>))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Aeson (FromJSON (..), Object, ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.CaseInsensitive
import Data.Maybe (fromMaybe)
import Data.Sequence
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Header)
import Network.URI.Extra (uriAuthToString, uriFromString, uriToString')
import Numeric.Natural (Natural)
import Sensei.Server.Auth (SerializedToken (..))
import Sensei.Version
import Servant hiding (Header)
import Servant.Client.Core

data SenseiClientConfig = SenseiClientConfig
  { serverUri :: URI
  , authToken :: Maybe SerializedToken
  , startServerLocally :: Bool
  , configUser :: Maybe Text
  -- ^ Name to use for querying server and registering new flows, notes, and commands
  -- If set to 'Nothing' the client will use the current (system) user name.
  --
  -- TODO: This should really not be here but handled as part of authentication and
  -- be stored in the token. If one user should be able to register flows for other
  -- users then this should be made an options somewhere.
  , apiVersion :: Maybe Version
  -- ^ The 'Version' to send to the server.
  -- If defined, this is sent in the `X-API-Version` header, otherwise client will
  -- send whatever version it's been build with.
  }
  deriving (Eq, Show)

instance ToJSON SenseiClientConfig where
  toJSON SenseiClientConfig{serverUri, authToken, startServerLocally, configUser, apiVersion} =
    object
      [ "serverUri" .= uriToString' serverUri
      , "authToken" .= authToken
      , "startServerLocally" .= startServerLocally
      , "configUser" .= configUser
      , "apiVersion" .= apiVersion
      ]

instance FromJSON SenseiClientConfig where
  parseJSON = withObject "SenseiClientConfig" $ \o -> do
    version <- (o .: "configVersion") <|> pure 0
    parseJSONFromVersion version o

parseJSONFromVersion :: Natural -> Object -> Parser SenseiClientConfig
parseJSONFromVersion 0 obj = do
  serverUri <-
    obj .: "serverUri" >>= \u ->
      case uriFromString u of
        Nothing -> fail $ "Invalid uri:" <> u
        Just uri -> pure uri
  authToken <- obj .: "authToken" <|> pure Nothing
  startServerLocally <- obj .: "startServerLocally"
  configUser <- obj .: "configUser" <|> pure Nothing
  apiVersion <- obj .: "apiVersion" <|> pure Nothing
  pure $ SenseiClientConfig{..}
parseJSONFromVersion n _ = fail $ "Unsupported version " <> show n

defaultConfig :: SenseiClientConfig
defaultConfig = SenseiClientConfig "http://localhost:23456" Nothing True Nothing Nothing

class ClientConfig config where
  defConfig :: config
  additionalHeaders :: config -> Seq Header -> Seq Header
  setServerUri :: URI -> config -> config
  getServerUri :: config -> URI
  setAuthToken :: Maybe SerializedToken -> config -> config
  getAuthToken :: config -> Maybe SerializedToken

instance ClientConfig SenseiClientConfig where
  defConfig = defaultConfig
  additionalHeaders SenseiClientConfig{apiVersion} rest =
    (mk "X-API-Version", toHeader $ fromMaybe senseiVersion apiVersion) <| rest
  setServerUri serverUri config = config{serverUri}
  getServerUri = serverUri
  setAuthToken authToken config = config{authToken}
  getAuthToken = authToken

newtype ClientMonad config a = ClientMonad {unClient :: forall m. RunClient m => ReaderT config m a}

instance Functor (ClientMonad config) where
  fmap f (ClientMonad a) = ClientMonad $ fmap f a

instance Applicative (ClientMonad config) where
  pure a = ClientMonad (pure a)
  ClientMonad f <*> ClientMonad a = ClientMonad $ f <*> a

instance Monad (ClientMonad config) where
  ClientMonad a >>= f =
    ClientMonad $ a >>= unClient . f

instance MonadReader config (ClientMonad config) where
  ask = ClientMonad $ ReaderT pure
  local f (ClientMonad ma) = ClientMonad $ local f ma

instance ClientConfig config => RunClient (ClientMonad config) where
  runRequestAcceptStatus st req = ClientMonad $ do
    config <- ask
    let hostPort = encodeUtf8 $ pack $ Prelude.drop 2 $ uriAuthToString id (uriAuthority $ getServerUri config) ""
        -- TODO: Find a better way to handle URIs
        authorization rest =
          maybe rest (\tok -> (mk "Authorization", "Bearer " <> unToken tok) <| rest) (getAuthToken config)
        request =
          req
            { requestHeaders =
                (mk "Host", hostPort)
                  <| (mk "Origin", encodeUtf8 $ pack $ uriToString' $ getServerUri config)
                  <| additionalHeaders config (authorization (requestHeaders req))
            }
    lift (runRequestAcceptStatus st request)

  throwClientError err = ClientMonad $ lift $ throwClientError err
