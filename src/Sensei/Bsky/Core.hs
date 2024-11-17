{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.Bsky.Core where

import Data.Aeson (FromJSON, ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

data BskyBackend = BskyBackend
  { login :: BskyLogin
  , pdsUrl :: URI
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BskyLogin = BskyLogin
  { identifier :: Text
  , password :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
