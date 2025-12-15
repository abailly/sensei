{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Sensei.Bsky.Core where

import Data.Aeson (FromJSON, ToJSON (..))
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import GHC.TypeLits (Symbol)

data BskyBackend = BskyBackend
  { login :: BskyLogin,
    pdsUrl :: URI
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BskyLogin = BskyLogin
  { identifier :: Text,
    password :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | The type of lexicon associated with a given record type
type family Lexicon (typ :: Type) :: Symbol

-- | The type of key associated with a given record type
type family Key (typ :: Type) :: Type
