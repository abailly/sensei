module Preface.CodecSpec where

import Preface.Codec
import Test.Hspec
import Test.QuickCheck
import Data.Proxy
import Test.QuickCheck.Classes
import qualified Data.ByteString as BS

instance Arbitrary (Encoded Hex) where
  arbitrary = toHex . BS.pack <$> arbitrary

instance Arbitrary (Encoded Base64) where
  arbitrary = toBase64 . BS.pack <$> arbitrary

spec :: Spec
spec = describe "Coding/Encoding 'strings'" $ do
  it "can serialise/deserialise Base16-encoded strings to/from JSON" $
    lawsCheck (jsonLaws (Proxy @(Encoded Hex)))

  it "can serialise/deserialise Base64-encoded strings to/from JSON" $
    lawsCheck (jsonLaws (Proxy @(Encoded Base64)))
