{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Preface.Codec
  ( Base64,
    Hex,
    Encoded (..),
    pattern EncodedHex,
    pattern EncodedBase64,
    encodedText,
    encodedHex,
    genRandomBase64,
    genRandomBaseHex,
    toBase64,
    fromBase64,
    encodeBase64,
    fromHex,
    toHex,
  )
where

import Crypto.Random.Entropy
import Data.Aeson hiding (decode, encode)
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import Data.Maybe (fromJust)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.ToText

data Base64

data Hex

class Codec base where
  encode :: Encoded base -> BS.ByteString
  decode :: BS.ByteString -> Maybe (Encoded base)

instance Codec Hex where
  encode (Encoded bs) = B16.encode bs
  decode bs = case B16.decode bs of
    Right u -> pure (Encoded u)
    _ -> Nothing

instance Codec Base64 where
  encode (Encoded bs) = B64.encode bs
  decode = either (const Nothing) (Just . Encoded) . B64.decode

data Encoded code = Encoded BS.ByteString
  deriving (Eq, Ord)

instance Show (Encoded Hex) where
  show (Encoded t) = fmap (toEnum . fromIntegral) $ BS.unpack (B16.encode t)

instance Show (Encoded Base64) where
  show (Encoded t) = fmap (toEnum . fromIntegral) $ BS.unpack (B64.encode t)

instance Read (Encoded Hex) where
  readsPrec n = fmap (first fromString) . readsPrec n

instance Codec base => ToJSON (Encoded base) where
  toJSON t = String (TE.decodeUtf8 (encode t))

instance Codec base => FromJSON (Encoded base) where
  parseJSON (String t) = case decode (TE.encodeUtf8 t) of
    Just r -> pure r
    _ -> fail $ "not a valid hexadecimal encoded string: " <> show t
  parseJSON v = fail $ "not a valid hexadecimal encoded string: " <> show v

instance Codec base => IsString (Encoded base) where
  fromString t = case decode (fromString t) of
    Just r -> r
    _ -> error $ "not a valid encoded string: " <> t

instance ToText (Encoded Hex) where
  toText (Encoded t) = TE.decodeUtf8 (B16.encode t)

instance ToText (Encoded Base64) where
  toText (Encoded t) = TE.decodeUtf8 (B64.encode t)

encodedText :: ToText a => a -> T.Text
encodedText = toText

encodedHex :: Encoded Hex -> T.Text
encodedHex = encodedText

encodedBase64 :: Encoded Base64 -> T.Text
encodedBase64 = encodedText

mkEncoded :: Codec base => T.Text -> Maybe (Encoded base)
mkEncoded = decode . TE.encodeUtf8

pattern EncodedHex :: T.Text -> Encoded Hex
pattern EncodedHex t <-
  (encodedHex -> t)
  where
    EncodedHex t = fromJust $ mkEncoded t

pattern EncodedBase64 :: T.Text -> Encoded Base64
pattern EncodedBase64 t <-
  (encodedBase64 -> t)
  where
    EncodedBase64 t = fromJust $ mkEncoded t

-- | Generates a cryptographically secure random string of given length
genRandomBase64 :: Int -> IO (Encoded Base64)
genRandomBase64 len = Encoded <$> getEntropy len

-- | Generates a cryptographically secure random string of given length producing only hexadecimal characters
genRandomBaseHex :: Int -> IO (Encoded Hex)
genRandomBaseHex len = Encoded <$> getEntropy len

toBase64 :: BS.ByteString -> Encoded Base64
toBase64 = Encoded

fromBase64 :: Encoded Base64 -> BS.ByteString
fromBase64 (Encoded t) = t

encodeBase64 :: BS.ByteString -> BS.ByteString
encodeBase64 = B64.encode

fromHex :: Encoded Hex -> BS.ByteString
fromHex (Encoded t) = t

toHex :: BS.ByteString -> Encoded Hex
toHex = Encoded
