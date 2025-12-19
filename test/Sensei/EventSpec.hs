module Sensei.EventSpec where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Either (isRight)
import Data.Proxy (Proxy (..))
import Sensei.API
import Sensei.Generators (generateEvent, startTime)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec
import Test.QuickCheck (Arbitrary, arbitrary)

instance Arbitrary Event where
  arbitrary = arbitrary >>= generateEvent startTime

spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @Event)

  it "can read events version 12" $ do
    events <- eitherDecodeFileStrict "golden/Event.v12.json" :: IO (Either String [Event])
    events `shouldSatisfy` isRight
