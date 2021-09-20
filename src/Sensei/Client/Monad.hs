{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.Client.Monad (ClientConfig (..), ClientMonad (..), module Control.Monad.Reader, defaultConfig) where

import Control.Applicative ((<|>))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Aeson (FromJSON (..), Object, ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.CaseInsensitive
import Data.Sequence
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.URI.Extra (uriAuthToString, uriFromString, uriToString')
import Numeric.Natural (Natural)
import Sensei.Server.Auth (SerializedToken (..))
import Sensei.Version
import Servant
import Servant.Client.Core

data ClientConfig = ClientConfig
  { serverUri :: URI,
    authToken :: Maybe SerializedToken,
    startServerLocally :: Bool,
    -- | Name to use for querying server and registering new flows, notes, and commands
    -- If set to 'Nothing' the client will use the current (system) user name.
    --
    -- TODO: This should really not be here but handled as part of authentication and
    -- be stored in the token. If one user should be able to register flows for other
    -- users then this should be made an options somewhere.
    configUser :: Maybe Text
  }
  deriving (Eq, Show)

instance ToJSON ClientConfig where
  toJSON ClientConfig {serverUri, authToken, startServerLocally, configUser} =
    object
      [ "serverUri" .= uriToString' serverUri,
        "authToken" .= authToken,
        "startServerLocally" .= startServerLocally,
        "configUser" .= configUser
      ]

instance FromJSON ClientConfig where
  parseJSON = withObject "ClientConfig" $ \o -> do
    version <- (o .: "configVersion") <|> pure 0
    parseJSONFromVersion version o

parseJSONFromVersion :: Natural -> Object -> Parser ClientConfig
parseJSONFromVersion 0 obj = do
  uri <-
    obj .: "serverUri" >>= \u ->
      case uriFromString u of
        Nothing -> fail $ "Invalid uri:" <> u
        Just uri -> pure uri
  token <- obj .: "authToken" <|> pure Nothing
  locally <- obj .: "startServerLocally"
  user <- obj .: "configUser" <|> pure Nothing
  pure $ ClientConfig uri token locally user
parseJSONFromVersion n _ = fail $ "Unsupported version " <> show n

defaultConfig :: ClientConfig
defaultConfig = ClientConfig "http://localhost:23456" Nothing True Nothing

newtype ClientMonad a = ClientMonad {unClient :: forall m. (RunClient m) => ReaderT ClientConfig m a}

instance Functor ClientMonad where
  fmap f (ClientMonad a) = ClientMonad $ fmap f a

instance Applicative ClientMonad where
  pure a = ClientMonad (pure a)
  ClientMonad f <*> ClientMonad a = ClientMonad $ f <*> a

instance Monad ClientMonad where
  ClientMonad a >>= f =
    ClientMonad $ a >>= unClient . f

instance MonadReader ClientConfig ClientMonad where
  ask = ClientMonad $ ReaderT pure
  local f (ClientMonad ma) = ClientMonad $ local f ma

instance RunClient ClientMonad where
  runRequestAcceptStatus st req = ClientMonad $ do
    ClientConfig {serverUri, authToken} <- ask
    let hostPort = encodeUtf8 $ pack $ Prelude.drop 2 $ uriAuthToString id (uriAuthority serverUri) ""
        -- TODO: Find a better way to handle URIs
        authorization rest =
          maybe rest (\tok -> (mk "Authorization", "Bearer " <> unToken tok) <| rest) authToken

        request =
          req
            { requestHeaders =
                (mk "Host", hostPort)
                  <| (mk "Origin", encodeUtf8 $ pack $ uriToString' serverUri)
                  <| (mk "X-API-Version", toHeader senseiVersion)
                  <| authorization (requestHeaders req)
            }
    lift (runRequestAcceptStatus st request)

  throwClientError err = ClientMonad $ lift $ throwClientError err
