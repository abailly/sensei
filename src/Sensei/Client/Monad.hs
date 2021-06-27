{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.Client.Monad (ClientConfig (..), ClientMonad (..), module Control.Monad.Reader, defaultConfig) where

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), withObject, (.:))
import Data.CaseInsensitive
import Data.Sequence
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Network.URI.Extra (uriFromString, uriToString', uriAuthToString)
import Sensei.Server.Auth.Types (SerializedToken (..))
import Sensei.Version
import Servant
import Servant.Client.Core

data ClientConfig = ClientConfig
  { serverUri :: URI,
    authToken :: Maybe SerializedToken,
    startServerLocally :: Bool
  }
  deriving (Eq, Show)

instance ToJSON ClientConfig where
  toJSON ClientConfig{serverUri, authToken, startServerLocally} =
    object [ "serverUri" .= uriToString' serverUri,
             "authToken" .= authToken,
             "startserverlocally" .= startServerLocally
           ]

instance FromJSON ClientConfig where
  parseJSON = withObject "ClientConfig" $ \o -> do
    uri <- o .: "serverUri" >>= \ u -> case uriFromString u of
                                         Nothing -> fail $ "Invalid uri:" <> u
                                         Just uri -> pure uri
    token <- o .: "authToken"
    locally <- o .: "startServerLocally"
    pure $ ClientConfig uri token locally
    
    
defaultConfig :: ClientConfig
defaultConfig = ClientConfig "http://localhost:23456" Nothing True

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
