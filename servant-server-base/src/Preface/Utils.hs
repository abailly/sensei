module Preface.Utils
  ( Hash (..),
    fromText,
    toText,
    PublicKey (..),
    PrivateKey (..),
    Signature (..),
    Hashable (..),
    decodeUtf8',
    (</>),
    hash,
    generateKeyPair,
    signWith,
    verifySignature,
  )
where

import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Digest, SHA1, digestFromByteString)
import qualified Crypto.Hash as H
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Random.Types (MonadRandom)
import qualified Data.Aeson as A
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.ToText
import Servant

-- | A SHA1 hash of some value
newtype Hash = Hash {hashValue :: Digest SHA1}
  deriving (Eq, Show, Ord)

instance IsString Hash where
  fromString = fromMaybe (error "invalid Hash string") . fromText . pack

instance A.ToJSON Hash where
  toJSON = A.String . toText

instance A.FromJSON Hash where
  parseJSON = A.withText "Hash" fromText

instance Semigroup Hash where
  Hash {hashValue} <> Hash {hashValue = hashValue'} =
    hash (convert hashValue <> convert hashValue')

instance FromHttpApiData Hash where
  parseUrlPiece t = maybe (Left $ "cannot convert " <> t <> " to Hash") Right $ fromText t

instance ToHttpApiData Hash where
  toUrlPiece = toText

fromText ::
  MonadFail m => Text -> m Hash
fromText s = case decode (encodeUtf8 s) of
  Right bs -> case digestFromByteString bs of
    Nothing -> fail $ "cannot decode hash from JSON: " <> show s
    Just h -> pure $ Hash h
  _ -> fail $ "cannot decode hash from JSON: " <> show s

instance ToText Hash where
  toText = decodeUtf8' . encode . convert . hashValue

-- | A class for things that we can compute a `Hash` from
class Hashable h where
  hashOf :: h -> Hash

-- | Parse a cryptographic key from a hex-encoded `Text`
parseKey ::
  MonadFail m =>
  (ByteString -> CryptoFailable t) ->
  (t -> a) ->
  Text ->
  m a
parseKey extractor ctor string =
  case (decode $ encodeUtf8 string) of
    Right bs -> case extractor bs of
      CryptoPassed k -> pure $ ctor k
      CryptoFailed _ -> fail $ "cannot decode key from JSON: " <> show string
    _ -> fail $ "cannot decode key from JSON: " <> show string

-- | A public key
-- The underlying implementation is opaque, here we use Ed25519 elliptic curve-based
-- key pairs
newtype PublicKey = PublicKey {publicKey :: Ed25519.PublicKey}
  deriving (Eq, Show)

instance A.ToJSON PublicKey where
  toJSON (PublicKey key) = A.String $ decodeUtf8' $ encode $ convert key

instance A.FromJSON PublicKey where
  parseJSON = A.withText "PublicKey" $ parseKey Ed25519.publicKey PublicKey

-- | A private key
-- Underlying implementation should be opaque but here we use explicitly Ed25519
-- elliptic curve key.
-- This is mostly used to generate key pairs in order to get valid public keys
-- and signatures.
newtype PrivateKey = PrivateKey {privateKey :: Ed25519.SecretKey}
  deriving (Eq, Show)

instance A.ToJSON PrivateKey where
  toJSON (PrivateKey key) = A.String $ decodeUtf8' $ encode $ convert key

instance A.FromJSON PrivateKey where
  parseJSON = A.withText "PrivateKey" $ parseKey Ed25519.secretKey PrivateKey

generateKeyPair :: (MonadRandom m) => m (PublicKey, PrivateKey)
generateKeyPair = do
  secret <- Ed25519.generateSecretKey
  pure (PublicKey (Ed25519.toPublic secret), PrivateKey secret)

data Signature
  = NotSigned
  | Signature {signature :: Ed25519.Signature}
  deriving (Eq, Show)

instance A.ToJSON Signature where
  toJSON NotSigned = A.String ""
  toJSON Signature {signature} = A.String $ decodeUtf8' $ encode $ convert signature

instance A.FromJSON Signature where
  parseJSON = A.withText "Signature" $
    \s ->
      if s == ""
        then pure NotSigned
        else parseKey Ed25519.signature Signature s

signWith ::
  PrivateKey -> PublicKey -> Hash -> Signature
signWith PrivateKey {privateKey} PublicKey {publicKey} Hash {hashValue} =
  Signature $ Ed25519.sign privateKey publicKey hashValue

verifySignature ::
  PublicKey -> Hash -> Signature -> Bool
verifySignature PublicKey {publicKey} Hash {hashValue} Signature {signature} =
  Ed25519.verify publicKey hashValue signature
verifySignature _ _ NotSigned = False

-- | Decode a `ByteString` into a `Text` assuming UTF-8 encoding.
-- If unknown bytes sequence are encountered they are replaced by a default
-- character.
decodeUtf8' :: ByteString -> Text
decodeUtf8' = decodeUtf8With lenientDecode

instance Hashable Text where
  hashOf = hash . encodeUtf8

instance Hashable ByteString where
  hashOf = hash

instance Hashable LBS.ByteString where
  hashOf = Hash . H.hashlazy

hash :: ByteString -> Hash
hash = Hash . H.hash

infixl 1 </>

-- | Helper to build URIs
(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b
