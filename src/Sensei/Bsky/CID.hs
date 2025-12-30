{-# LANGUAGE OverloadedStrings #-}

module Sensei.Bsky.CID
  ( CID,
    CIDError (..),
    computeCID,
    cidToText,
    textToCID,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32 as Base32
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)

-- | Content Identifier (CID) version 1
-- Format: <multibase-prefix><version><multicodec><multihash>
newtype CID = CID ByteString
  deriving (Eq, Show)

-- Multicodec codes
rawCodec :: Word8
rawCodec = 0x55 -- raw binary

-- Multihash codes
sha256Code :: Word8
sha256Code = 0x12 -- SHA-256

-- CID version
cidV1 :: Word8
cidV1 = 0x01

-- | Compute a CIDv1 for the given binary data
-- Uses raw codec and SHA-256 hash
computeCID :: ByteString -> CID
computeCID content =
  let -- Compute SHA-256 hash
      digest :: Digest SHA256
      digest = hash content
      hashBytes = convert digest :: ByteString
      hashLen = fromIntegral $ BS.length hashBytes :: Word8

      -- Build multihash: <hash-function-code><digest-length><digest-bytes>
      multihash = BS.cons sha256Code $ BS.cons hashLen hashBytes

      -- Build CID: <version><codec><multihash>
      cidBytes = BS.cons cidV1 $ BS.cons rawCodec multihash
   in CID cidBytes

-- | Convert CID to text representation using base32 encoding
-- Prepends 'b' multibase prefix for base32lower
cidToText :: CID -> Text
cidToText (CID bytes) =
  -- 'b' prefix indicates base32 lowercase encoding
  "b" <> Text.toLower (Base32.encodeBase32Unpadded bytes)

data CIDError
  = EmptyCID
  | InvalidPrefix {found :: Text}
  | DecodeError {found :: Text, reason :: Text}
  | UnsupportedVersion {found :: Text, foundVersion :: Word8}
  deriving (Eq, Show)

-- | Parse CID from text representation
-- Expects base32 encoding with 'b' prefix
textToCID :: Text -> Either CIDError CID
textToCID txt
  | Text.null txt = Left EmptyCID
  | Text.head txt /= 'b' = Left (InvalidPrefix txt)
  | otherwise =
      case Base32.decodeBase32Unpadded (encodeUtf8 $ Text.tail txt) of
        Left err -> Left $ DecodeError { found = txt, reason = err }
        Right bytes
          | BS.null bytes -> Left EmptyCID
          | BS.head bytes /= cidV1 -> Left (UnsupportedVersion {found = txt, foundVersion = BS.head bytes})
          | otherwise -> Right $ CID bytes
