
module Sensei.Bsky.TIDSpec where

import qualified Data.Text as Text
import Sensei.Bsky.TID (mkTid, tidFromText, tidToText)
import Sensei.Generators ()
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    shouldNotBe,
    shouldSatisfy,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

spec :: Spec
spec = do
  describe "TID generation" $ do
    it "generates a 13-character TID" $ do
      tid <- mkTid
      let tidText = tidToText tid
      Text.length tidText `shouldBe` 13

    it "generates TIDs using only base32-sortable characters" $ do
      tid <- mkTid
      let tidText = tidToText tid
          validChars = "234567abcdefghijklmnopqrstuvwxyz" :: String
      all (`elem` validChars) (Text.unpack tidText) `shouldBe` True

    it "generates different TIDs on subsequent calls" $ do
      tid1 <- mkTid
      tid2 <- mkTid
      tid1 `shouldNotBe` tid2

  describe "tidFromText" $ do
    prop "parses a valid TID" $ \tid ->
      let tidText = tidToText tid
       in tidFromText tidText === Just tid

    it "rejects TID with invalid length" $ do
      tidFromText "short" `shouldBe` Nothing
      tidFromText "waytoolongstring" `shouldBe` Nothing

    it "rejects TID with invalid characters" $ do
      tidFromText "1234567890xyz" `shouldBe` Nothing
      tidFromText "ABCDEFGHIJKLM" `shouldBe` Nothing

    it "accepts TID with valid characters" $ do
      let validTid = "2222222222222" -- The zero value
      tidFromText validTid `shouldSatisfy` (/= Nothing)
