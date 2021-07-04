{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.UserSpec where

import Data.Aeson
import qualified Data.Map as Map
import Data.Proxy
import Sensei.API
import Sensei.ColorSpec ()
import Sensei.Generators ()
import Sensei.TestHelper
import Test.Hspec
import Test.QuickCheck.Classes

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
        `shouldBe` Right defaultProfile {userFlowTypes = Just (Map.fromList [(FlowType "Experimenting", "#ba83dc")])}

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

    it "can deserialize version 5 JSON" $ do
      let jsonProfile = "{\"userStartOfDay\":\"08:00:00\",\"userCommands\":null,\"userProfileVersion\":5,\"userEndOfDay\":\"18:30:00\",\"userName\":\"arnaud\",\"userTimezone\":\"+01:00\",\"userFlowTypes\":null}"
      eitherDecode jsonProfile
        `shouldBe` Right defaultProfile

  withApp app $
    describe "Users API" $ do
      it "GET /api/users/<user> returns default profile" $ do
        getJSON "/api/users/arnaud" `shouldRespondJSONBody` defaultProfile

      it "PUT /api/users/<user> sets user profile" $ do
        let profile = defaultProfile {userName = "robert"}

        putJSON_ "/api/users/arnaud" profile

        getJSON "/api/users/arnaud" `shouldRespondJSONBody` profile

      it "PUT /api/users/<user> sets hashed user's password in profile" $ do
        let profile = defaultProfile {userPassword = ("1234", "1234")}

        putJSON_ "/api/users/arnaud" profile

        getJSON "/api/users/arnaud" `shouldRespondJSONBody` profile
