{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.Client.Monad where

import Data.CaseInsensitive
import Data.Sequence
import Servant
import Sensei.Version
import Servant.Client.Core

newtype ClientMonad a = ClientMonad {unClient :: forall m. (RunClient m) => m a}

instance Functor ClientMonad where
  fmap f (ClientMonad a) = ClientMonad $ fmap f a

instance Applicative ClientMonad where
  pure a = ClientMonad (pure a)
  ClientMonad f <*> ClientMonad a = ClientMonad $ f <*> a

instance Monad ClientMonad where
  ClientMonad a >>= f =
    ClientMonad $ a >>= unClient . f

instance RunClient ClientMonad where
  runRequestAcceptStatus st req = ClientMonad $ do
    let request =
          req
            { requestHeaders =
                (mk "Host", "localhost:23456")
                  <| (mk "Origin", "http://localhost:23456")
                  <| (mk "X-API-Version", toHeader senseiVersion)
                  <| requestHeaders req
            }
    runRequestAcceptStatus st request

  throwClientError err = ClientMonad $ throwClientError err
