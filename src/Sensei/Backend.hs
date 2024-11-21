{-# LANGUAGE GADTs #-}

module Sensei.Backend where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Typeable (Typeable)

data Backend where
  Backend :: (Show backend, ToJSON backend, FromJSON backend, Typeable backend) => backend -> Backend

instance Eq Backend where
  Backend backend == Backend backend' = toJSON backend == toJSON backend'

instance Show Backend where
  show (Backend backend) = show backend

-- NOTE: There's no FromJSON instance because it's not possible to construct
-- one for an existential type like `Backend`: While you know the contained
-- value has a `FromJSON` instance, you can't tell which one so you cannot
-- construct a `Backend` value.
instance ToJSON Backend where
  toJSON (Backend backend) = toJSON backend
