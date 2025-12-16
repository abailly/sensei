{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sensei.Bsky.LeafletSpec where

import Data.Data (Proxy (..))
import Sensei.Bsky.Leaflet (Document)
import Sensei.Generators ()
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (Spec)

spec :: Spec
spec =
  roundtripAndGoldenSpecs (Proxy @Document)
