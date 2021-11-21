{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.UserSpec where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Proxy
import qualified Data.Text.Encoding as UTF8
import Preface.Codec (Encoded (..), encodedHex, toHex)
import Sensei.API
import Sensei.Client (getUserProfileC)
import Sensei.ColorSpec ()
import Sensei.Generators ()
import Sensei.TestHelper
import Sensei.WaiTestHelper (matches, runRequest)
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

    it "can deserialize version 6 JSON" $ do
      let jsonProfile = "{\"userStartOfDay\":\"08:00:00\",\"userCommands\":null,\"userProfileVersion\":6,\"userEndOfDay\":\"18:30:00\",\"userName\":\"arnaud\",\"userTimezone\":\"+01:00\",\"userFlowTypes\":null,\"userPassword\":[\"\",\"\"]}"
      eitherDecode jsonProfile
        `shouldBe` Right defaultProfile

    it "can deserialize version 7 JSON" $ do
      let uid = toHex "foo"
          jsonProfile = "{\"userStartOfDay\":\"08:00:00\",\"userCommands\":null,\"userProfileVersion\":7,\"userEndOfDay\":\"18:30:00\",\"userName\":\"arnaud\",\"userTimezone\":\"+01:00\",\"userFlowTypes\":null,\"userPassword\":[\"\",\"\"],\"userId\":\"" <> LBS.fromStrict (UTF8.encodeUtf8 (encodedHex uid)) <> "\"}"
      eitherDecode jsonProfile
        `shouldBe` Right defaultProfile {userId = uid}

  withApp app $
    describe "Users API" $ do
      it "GET /api/users/<user> returns default profile" $ do
        getJSON "/api/users/arnaud"
          `shouldMatchJSONBody` \p -> p {userId = ""} == defaultProfile

      it "GET /api/users/<user> returns default profile with user id" $ do
        uid <- userId <$> runRequest getUserProfileC
        uid `matches` \(Encoded e) -> BS.length e == 16

      it "POST /api/users with profile sets create user profile and returns user id" $ do
        let profile = defaultProfile {userName = "robert"}

        postJSON "/api/users" profile
          `shouldRespondWith` 200 {matchBody = bodySatisfies $ \bs -> BS.length bs == 32 + 2}

      it "POST /api/users with profile returns 400 given user with same name exists" $ do
        let profile = defaultProfile {userName = "robert"}

        postJSON_ "/api/users" profile

        postJSON "/api/users" profile
          `shouldRespondWith` 400

      it "PUT /api/users/<user> sets user profile given user exists" $ do
        let profile = defaultProfile {userName = "robert", userFlowTypes = Just $ Map.fromList [(Other, "#123456")]}
        postJSON_ "/api/users" profile

        putJSON "/api/users/robert" profile `shouldRespondWith` 200
        getJSON "/api/users/robert" `shouldRespondJSONBody` profile

      it "PUT /api/users/<user> returns 400 given profile user name does not match path" $ do
        let alice = defaultProfile {userName = "alice"}
            robert = defaultProfile {userName = "robert"}

        postJSON_ "/api/users" alice
        postJSON_ "/api/users" robert

        putJSON "/api/users/alice" robert
          `shouldRespondWith` 400

      it "PUT /api/users/<user> sets hashed user's password in profile" $ do
        let profile = defaultProfile {userPassword = ("1234", "1234")}

        putJSON_ "/api/users/arnaud" profile

        getJSON "/api/users/arnaud" `shouldRespondJSONBody` profile
