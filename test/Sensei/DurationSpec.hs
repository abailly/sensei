{-# LANGUAGE TypeApplications #-}
module Sensei.DurationSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes
import Sensei.API
import Data.Proxy

instance Arbitrary TimeDifference where
  arbitrary = Minutes <$> arbitrary

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
