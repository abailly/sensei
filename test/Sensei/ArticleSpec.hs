module Sensei.ArticleSpec where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isInfixOf)
import Sensei.API
import Sensei.Builder (aDay)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), elements)

instance Arbitrary Article where
  arbitrary = do
    user <- elements ["alice", "bob", "charlie"]
    let timestamp = UTCTime aDay 0
    let dir = "/some/directory"
    pure $ PublishArticle user timestamp dir "" -- TODO

spec :: Spec
spec = describe "Article" $ do
  describe "Article" $ do
    prop "round-trips through JSON" $ \art ->
      decode (encode art) == Just (art :: Article)

    it "serializes Article to JSON with correct fields" $ do
      let op = PublishArticle
            { _articleUser = "testuser"
            , _articleTimestamp = UTCTime aDay 0
            , _articleDir = "/test/dir"
            , _article = ""
            }
      let encoded = LBS.unpack $ encode op
      ("testuser" `isInfixOf` encoded) `shouldBe` True
