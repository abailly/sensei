module Sensei.ArticleSpec where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isInfixOf)
import Sensei.API
import Sensei.Builder (aDay)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), elements)

instance Arbitrary ArticleOperation where
  arbitrary = pure Publish

instance Arbitrary ArticleOp where
  arbitrary = do
    op <- arbitrary
    user <- elements ["alice", "bob", "charlie"]
    let timestamp = UTCTime aDay 0
    let dir = "/some/directory"
    pure $ ArticleOp op user timestamp dir "" -- TODO

spec :: Spec
spec = describe "Article" $ do
  describe "ArticleOperation" $ do
    it "serializes Publish to JSON" $ do
      encode Publish `shouldBe` "\"Publish\""

    it "deserializes Publish from JSON" $ do
      decode "\"Publish\"" `shouldBe` Just Publish

  describe "ArticleOp" $ do
    prop "round-trips through JSON" $ \articleOp ->
      decode (encode articleOp) == Just (articleOp :: ArticleOp)

    it "serializes ArticleOp to JSON with correct fields" $ do
      let op = ArticleOp
            { _articleOperation = Publish
            , _articleUser = "testuser"
            , _articleTimestamp = UTCTime aDay 0
            , _articleDir = "/test/dir"
            , _article = ""
            }
      let encoded = LBS.unpack $ encode op
      ("articleOperation" `isInfixOf` encoded &&
        "Publish" `isInfixOf` encoded &&
        "testuser" `isInfixOf` encoded) `shouldBe` True
