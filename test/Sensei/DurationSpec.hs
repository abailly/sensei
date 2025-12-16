
module Sensei.DurationSpec where

import Data.Proxy
import Sensei.API
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes

instance Arbitrary TimeDifference where
  arbitrary = oneof [Minutes <$> arbitrary, Hours <$> arbitrary]

parsePrettyPrinted :: TimeDifference -> Bool
parsePrettyPrinted td =
  let pp = prettyPrint td
   in parse pp == Right td

spec :: Spec
spec = describe "Time Difference" $ do
  it "can serialise/deserialise to/from JSON" $
    lawsCheck (jsonLaws (Proxy @TimeDifference))

  it "can parse/pretty-print" $
    property $ parsePrettyPrinted
