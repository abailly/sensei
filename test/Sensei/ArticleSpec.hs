module Sensei.ArticleSpec where

import Data.Data (Proxy (..))
import Sensei.API
import Sensei.Generators (generateArticle, startTime)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec
import Test.QuickCheck (Arbitrary (..), Positive (getPositive))

instance Arbitrary Article where
  arbitrary = do
    idx <- getPositive <$> arbitrary
    generateArticle startTime idx

spec :: Spec
spec =
  roundtripAndGoldenSpecs (Proxy @Article)
