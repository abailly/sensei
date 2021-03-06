{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.UserSpec where

import Data.Aeson
import qualified Data.Map as Map
import Data.Proxy
import Data.Text (Text, pack)
import Data.Time.LocalTime
import Sensei.API
import Sensei.ColorSpec ()
import Sensei.DB.Model ()
import Sensei.TestHelper
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes

-- * Orphan Instances

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay <$> choose (0, 11) <*> choose (0, 59) <*> (fromInteger <$> choose (0, 59))

instance Arbitrary TimeZone where
  arbitrary = hoursToTimeZone <$> choose (- 12, 12)

generateUser :: Gen Text
generateUser = resize 20 (pack . getASCIIString <$> arbitrary)

instance Arbitrary UserProfile where
  arbitrary =
    UserProfile
      <$> generateUser
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

spec :: Spec
spec = describe "Users Management" $ do
  describe "User Profile" $ do
    it "can serialise/deserialise to/from JSON" $
      lawsCheck (jsonLaws (Proxy @UserProfile))

    it "can deserialize version 0 JSON" $ do
      let jsonProfile = "{\"userStartOfDay\":\"08:00:00\",\"userEndOfDay\":\"18:30:00\",\"userName\":\"arnaud\", \"userTimezone\":\"+01:00\"}"
      eitherDecode jsonProfile
        `shouldBe` Right defaultProfile {userFlowTypes = Nothing}

    it "can deserialize version 1 JSON" $ do
      let jsonProfile = "{\"userStartOfDay\":\"08:00:00\",\"userEndOfDay\":\"18:30:00\",\"userName\":\"arnaud\",\"userProfileVersion\":1, \"userTimezone\":\"+01:00\",\"userFlowTypes\":[\"Experimenting\"]}"
      eitherDecode jsonProfile
        `shouldBe` Right defaultProfile {userFlowTypes = Just (Map.fromList [(FlowType "Experimenting", "#010aab")])}

    it "can deserialize version 2 JSON" $ do
      let jsonProfile = "{\"userStartOfDay\":\"08:00:00\",\"userProfileVersion\":2,\"userEndOfDay\":\"18:30:00\",\"userName\":\"arnaud\",\"userTimezone\":\"+01:00\",\"userFlowTypes\":[[\"Experimenting\",\"#0022dd\"]]}"
      eitherDecode jsonProfile
        `shouldBe` Right defaultProfile {userFlowTypes = Just (Map.fromList [(FlowType "Experimenting", "#0022dd")])}

    it "can deserialize version 3 JSON" $ do
      let jsonProfile = "{\"userStartOfDay\":\"08:00:00\",\"userProfileVersion\":3,\"userEndOfDay\":\"18:30:00\",\"userName\":\"arnaud\",\"userTimezone\":\"+01:00\",\"userFlowTypes\":{\"Experimenting\":\"#0022dd\"}}"
      eitherDecode jsonProfile
        `shouldBe` Right defaultProfile {userFlowTypes = Just (Map.fromList [(FlowType "Experimenting", "#0022dd")])}

    it "can deserialize version 4 JSON" $ do
      let jsonProfile = "{\"userStartOfDay\":\"08:00:00\",\"userProfileVersion\":4,\"userEndOfDay\":\"18:30:00\",\"userName\":\"arnaud\",\"userTimezone\":\"+01:00\",\"userFlowTypes\":{\"Experimenting\":\"#0022dd\"},\"userCommands\":{\"foo\":\"/usr/bin/foo\"}}"
      eitherDecode jsonProfile
        `shouldBe` Right
          defaultProfile
            { userCommands = Just (Map.fromList [("foo", "/usr/bin/foo")]),
              userFlowTypes = Just (Map.fromList [(FlowType "Experimenting", "#0022dd")])
            }

  withApp app $
    describe "Users API" $ do
      it "GET /api/users/<user> returns defualt profile" $ do
        getJSON "/api/users/arnaud"
          `shouldRespondWith` ResponseMatcher 200 [] (jsonBodyEquals defaultProfile)

      it "PUT /api/users/<user> sets user profile" $ do
        let profile =
              UserProfile
                { userName = "robert",
                  userTimezone = hoursToTimeZone 1,
                  userStartOfDay = TimeOfDay 08 00 00,
                  userEndOfDay = TimeOfDay 18 30 00,
                  userFlowTypes = Nothing,
                  userCommands = Just (Map.fromList [("g", "/usr/bin/git")])
                }
        putJSON_ "/api/users/arnaud" profile

        getJSON "/api/users/arnaud"
          `shouldRespondWith` ResponseMatcher 200 [] (jsonBodyEquals profile)
