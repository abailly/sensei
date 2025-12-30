{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Sensei.Bsky.Core where

import Data.Aeson (FromJSON, ToJSON (..), Value, object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Numeric.Natural (Natural)
import Sensei.Bsky.CID (CID, cidToText, textToCID)
import Servant

newtype DID = DID Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, ToJSON, FromJSON)

newtype AtURI = AtURI Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, ToJSON, FromJSON)

data BskyBackend = BskyBackend
  { login :: BskyLogin,
    pdsUrl :: URI,
    -- | The DID for this user
    -- TODO: define/use a proper DID type
    -- ipld-cid has one but the library is unmaintained and has bitrotten
    userDID :: DID,
    -- | The leaflet.pub publication ID for this user
    -- Seems like URI does not like `at://` scheme
    publicationId :: AtURI
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

parseBskyBackendFromVersion :: Natural -> Value -> Parser BskyBackend
parseBskyBackendFromVersion v =
  if v < 12
    then parseBackendV11
    else parseBackendV12
  where
    parseBackendV11 = withObject "BskyBackend v11" $ \o ->
      BskyBackend
        <$> o .: "login"
        <*> o .: "pdsUrl"
        <*> pure ""
        <*> pure ""

    parseBackendV12 = withObject "BskyBackend v12" $ \o ->
      BskyBackend
        <$> o .: "login"
        <*> o .: "pdsUrl"
        <*> o .: "userDID"
        <*> o .: "publicationId"

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

instance FromHttpApiData BskyHandle where
  parseQueryParam = Right . BskyHandle

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

instance (KnownSymbol bskyType) => FromHttpApiData (BskyType bskyType) where
  parseQueryParam txt =
    if txt == Text.pack (symbolVal (Proxy :: Proxy bskyType))
      then Right BskyType
      else Left $ "Expected " <> Text.pack (symbolVal (Proxy :: Proxy bskyType)) <> " but got " <> txt

data BskyRecord record = BskyRecord
  { repo :: BskyHandle,
    collection :: BskyType (Lexicon record),
    rkey :: Key record,
    record :: Maybe record
  }
  deriving stock (Generic)

deriving instance (Show record, KnownSymbol (Lexicon record), Show (Key record)) => Show (BskyRecord record)

deriving instance (Eq record, Eq (Key record)) => Eq (BskyRecord record)

instance (ToJSON record, ToJSON (Key record), KnownSymbol (Lexicon record)) => ToJSON (BskyRecord record) where
  toJSON (BskyRecord repo' coll key rec) =
    object $
      catMaybes
        [ Just ("repo" .= repo'),
          Just ("collection" .= coll),
          Just ("rkey" .= key),
          ("record" .=) <$> rec
        ]

instance (FromJSON record, FromJSON (Key record), KnownSymbol (Lexicon record)) => FromJSON (BskyRecord record) where
  parseJSON = withObject "BskyRecord" $ \o ->
    BskyRecord
      <$> o .: "repo"
      <*> o .: "collection"
      <*> o .: "rkey"
      <*> o .:? "record"

-- | A record with its metadata (uri, cid, value)
data RecordWithMetadata record = RecordWithMetadata
  { uri :: Text,
    cid :: Text,
    value :: record
  }
  deriving stock (Generic)

deriving instance (Show record) => Show (RecordWithMetadata record)

deriving instance (Eq record) => Eq (RecordWithMetadata record)

instance (FromJSON record) => FromJSON (RecordWithMetadata record) where
  parseJSON = withObject "RecordWithMetadata" $ \o ->
    RecordWithMetadata
      <$> o .: "uri"
      <*> o .: "cid"
      <*> o .: "value"

instance (ToJSON record) => ToJSON (RecordWithMetadata record) where
  toJSON (RecordWithMetadata uri' cid' value') =
    object
      [ "uri" .= uri',
        "cid" .= cid',
        "value" .= value'
      ]

type Sendable record =
  ( MimeRender JSON record,
    ToJSON record,
    ToJSON (Key record),
    KnownSymbol (Lexicon record)
  )

-- | Blob type as defined in AT Protocol spec
-- Represents a reference to binary content stored in the repository
-- See: https://atproto.com/specs/data-model#blob-type
data Blob = Blob
  { -- | MIME type of the blob content
    mimeType :: Text,
    -- | Size of the blob in bytes
    size :: Int,
    -- | CID reference to the blob content
    ref :: BlobRef
  }
  deriving stock (Eq, Show, Generic)

type instance Lexicon Blob = "blob"

instance ToJSON Blob where
  toJSON Blob {mimeType, size, ref} =
    object
      [ "$type" .= BskyType @(Lexicon Blob),
        "mimeType" .= mimeType,
        "size" .= size,
        "ref" .= ref
      ]

instance FromJSON Blob where
  parseJSON = withObject "Blob" $ \v -> do
    typ <- v .: "$type" :: Parser Text
    case typ of
      "blob" ->
        Blob
          <$> v .: "mimeType"
          <*> v .: "size"
          <*> v .: "ref"
      _ -> fail $ "Expected blob type, got: " ++ show typ

-- | Reference to blob content using CID
newtype BlobRef = BlobRef {link :: CID}
  deriving stock (Eq, Show, Generic)

instance ToJSON BlobRef where
  toJSON (BlobRef cid) = object ["$link" .= cidToText cid]

instance FromJSON BlobRef where
  parseJSON = withObject "BlobRef" $ \v -> do
    cidText <- v .: "$link"
    case textToCID cidText of
      Left err -> fail $ "Failed to parse CID: " <> show err
      Right cid -> pure $ BlobRef cid

-- | Response from uploadBlob endpoint
-- Contains the blob reference with CID
data BlobUploadResponse = BlobUploadResponse
  { blob :: BlobMetadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Metadata for an uploaded blob
-- FIXME: This is just a `Blob`
data BlobMetadata = BlobMetadata
  { blobRef :: Text, -- CID reference
    blobMimeType :: Text,
    blobSize :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BlobMetadata where
  toJSON BlobMetadata {blobRef, blobMimeType, blobSize} =
    object
      [ "$type" .= BskyType @(Lexicon Blob),
        "ref" .= object ["$link" .= blobRef],
        "mimeType" .= blobMimeType,
        "size" .= blobSize
      ]

instance FromJSON BlobMetadata where
  parseJSON = withObject "BlobMetadata" $ \v -> do
    refObj <- v .: "ref"
    ref <- withObject "ref" (.: "$link") refObj
    BlobMetadata ref
      <$> v .: "mimeType"
      <*> v .: "size"
