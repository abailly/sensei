{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.Client.Monad
( ClientConfig(..), ClientMonad(..), module Control.Monad.Reader)
  where

import Control.Monad.Reader(ReaderT(..), MonadReader(..))
import Control.Monad.Trans(MonadTrans(..))
import Data.CaseInsensitive
import Data.Sequence
import Sensei.Version
import Sensei.Server.Auth.Types(SerializedToken(..))
import Servant
import Servant.Client.Core
import Data.Text(pack)
import Data.Text.Encoding(encodeUtf8)

data ClientConfig =
  ClientConfig { serverHost :: String,
                 serverPort :: Int,
                 authToken :: Maybe SerializedToken
               }
  deriving (Eq, Show)

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
    ClientConfig{serverHost,serverPort,authToken} <- ask
    let hostPort = encodeUtf8 $ pack $ serverHost <> ":" <> show serverPort
        authorization rest =
          maybe rest (\ tok -> (mk "Authorization", "Bearer " <> unToken tok) <| rest) authToken

        request =
          req
            { requestHeaders =
                (mk "Host", hostPort)
                  <| (mk "Origin", "http://" <> hostPort)
                  <| (mk "X-API-Version", toHeader senseiVersion)
                  <| authorization (requestHeaders req)
            }
    lift (runRequestAcceptStatus st request)

  throwClientError err = ClientMonad $ lift $ throwClientError err
