{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.Backend where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Sensei.Bsky.Core (BskyBackend)

data Backend where
  Backend :: (Show backend, ToJSON backend, FromJSON backend) => backend -> Backend

instance Eq Backend where
  Backend backend == Backend backend' = toJSON backend == toJSON backend'

instance Show Backend where
  show (Backend backend) = show backend

instance ToJSON Backend where
  toJSON (Backend backend) = toJSON backend

instance FromJSON Backend where
  parseJSON v = Backend <$> parseJSON @BskyBackend v
