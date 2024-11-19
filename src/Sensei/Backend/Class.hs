{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Sensei.Backend.Class where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Sensei.Client.Monad (ClientMonad, Config)
import Sensei.Event (Event)

data BackendIO backend m = BackendIO
  { send :: forall a. Config backend -> ClientMonad (Config backend) a -> m a
  }

class (ToJSON backend, FromJSON backend, Show backend) => IsBackend backend where
  postEvent :: Monad m => backend -> BackendIO backend m -> Event -> m ()
