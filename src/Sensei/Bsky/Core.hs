{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sensei.Bsky.Core where

import Data.Aeson (FromJSON, ToJSON (..), withText, (.=), withObject, (.:), object)
import Data.Kind (Type)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Network.URI.Extra (uriFromString)
import Servant
import Data.String (IsString)
import Data.Aeson.Types (FromJSON(..))
import qualified Data.Text as Text

data BskyBackend = BskyBackend
  { login :: BskyLogin,
    pdsUrl :: URI
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkBackend :: Text -> Text -> Text -> Maybe BskyBackend
mkBackend ident pwd url = do
  pdsUrl <- uriFromString (unpack url)
  pure $
    BskyBackend
      { login = BskyLogin {identifier = ident, password = pwd},
        pdsUrl
      }

data BskyLogin = BskyLogin
  { identifier :: Text,
    password :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | The type of lexicon associated with a given record type
type family Lexicon (typ :: Type) :: Symbol

-- | The type of key associated with a given record type
type family Key (typ :: Type) :: Type

newtype BskyHandle = BskyHandle Text
  deriving newtype (Eq, Show, IsString, ToJSON, FromJSON)

instance ToHttpApiData BskyHandle where
  toUrlPiece (BskyHandle handle) = handle

data BskyType (bskyType :: Symbol) = BskyType

instance (KnownSymbol bskyType) => Show (BskyType bskyType) where
  show _ = symbolVal (Proxy :: Proxy bskyType)

instance Eq (BskyType bskyType) where
  _ == _ = True

instance (KnownSymbol bskyType) => ToJSON (BskyType bskyType) where
  toJSON _ = toJSON $ symbolVal (Proxy :: Proxy bskyType)

instance (KnownSymbol bskyType) => FromJSON (BskyType bskyType) where
  parseJSON = withText "Bsky type" $ \txt ->
    if txt == Text.pack (symbolVal (Proxy :: Proxy bskyType))
      then pure BskyType
      else fail ("unexpected Bsky type " <> Text.unpack txt)

instance (KnownSymbol bskyType) => ToHttpApiData (BskyType bskyType) where
  toUrlPiece _ = Text.pack $ symbolVal (Proxy :: Proxy bskyType)

data BskyRecord record = BskyRecord
  { repo :: BskyHandle,
    collection :: BskyType (Lexicon record),
    key :: Key record,
    record :: record
  }
  deriving stock (Generic)

deriving instance (Show record, KnownSymbol (Lexicon record), Show (Key record)) => Show (BskyRecord record)

deriving instance (Eq record, Eq (Key record)) => Eq (BskyRecord record)

instance (ToJSON record, ToJSON (Key record), KnownSymbol (Lexicon record)) => ToJSON (BskyRecord record) where
  toJSON (BskyRecord repo coll key rec) =
    object
      [ "repo" .= repo,
        "collection" .= coll,
        "key" .= key,
        "record" .= rec
      ]

instance (FromJSON record, FromJSON (Key record), KnownSymbol (Lexicon record)) => FromJSON (BskyRecord record) where
  parseJSON = withObject "BskyRecord" $ \o ->
    BskyRecord
      <$> o .: "repo"
      <*> o .: "collection"
      <*> o .: "key"
      <*> o .: "record"
