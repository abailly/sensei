{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Implementation of Timestamp Identifiers (TIDs) as specified by AT Protocol.
--
-- See [BlueSky specification](https://atproto.com/specs/tid).
module Sensei.Bsky.TID
  ( TID (..),
    tid,
    base32SortableAlphabet,
    mkTid,
    tidToText,
    tidFromText,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Random (randomRIO)

-- | Timestamp Identifier as specified by AT Protocol.
-- TIDs are 64-bit values encoded as 13-character base32-sortable strings.
newtype TID = TID {unTID :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, ToJSON, FromJSON)

-- | Create a TID from a timestamp and a clock identifier.
-- The TID format is:
--   - Top bit: 0 (always)
--   - Next 53 bits: Microseconds since UNIX epoch
--   - Final 10 bits: Random clock identifier
tid :: (RealFrac p) => p -> Word64 -> TID
tid posixTime clockId =
  -- Construct the 64-bit TID value
  -- Top bit is 0, next 53 bits are timestamp, last 10 bits are clock ID
  let microseconds = round (posixTime * 1000000) :: Word64
      timestamp = microseconds .&. 0x1FFFFFFFFFFFFF -- Mask to 53 bits
      tidValue = (timestamp `shiftL` 10) .|. clockId
   in TID $ encodeBase32Sortable tidValue

-- | Base32-sortable alphabet as specified by AT Protocol.
-- Uses characters that are lexicographically sortable.
base32SortableAlphabet :: Text
base32SortableAlphabet = "234567abcdefghijklmnopqrstuvwxyz"

-- | Convert a character index (0-31) to the corresponding base32-sortable character.
indexToChar :: Int -> Char
indexToChar = Text.index base32SortableAlphabet

-- | Encode a 64-bit value as a 13-character base32-sortable string.
encodeBase32Sortable :: Word64 -> Text
encodeBase32Sortable value = Text.pack $ reverse $ go value 13
  where
    go :: Word64 -> Int -> [Char]
    go _ 0 = []
    go v n =
      let idx = fromIntegral (v .&. 0x1F) -- Extract lowest 5 bits
          char = indexToChar idx
       in char : go (v `shiftR` 5) (n - 1)

-- | Create a new TID with the current timestamp and a random clock identifier.
mkTid :: IO TID
mkTid =
  tid <$> getPOSIXTime <*> randomRIO (0, 1023)

-- | Convert a TID to its text representation.
tidToText :: TID -> Text
tidToText = unTID

-- | Parse a TID from text.
-- Returns Nothing if the text is not a valid TID format.
tidFromText :: Text -> Maybe TID
tidFromText txt
  | Text.length txt == 13 && Text.all (`Text.elem` base32SortableAlphabet) txt =
      Just (TID txt)
  | otherwise = Nothing
