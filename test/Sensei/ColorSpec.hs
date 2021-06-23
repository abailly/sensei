{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.ColorSpec where

import Data.Colour.SRGB
import Data.Proxy
import Sensei.API
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes

instance Arbitrary Color where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    pure $ Color $ sRGB24 r g b

spec :: Spec
spec = describe "Color" $ do
  it "can serialise/deserialise Color to/from JSON" $
    lawsCheck (jsonLaws (Proxy @Color))
