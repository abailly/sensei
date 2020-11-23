{-# LANGUAGE OverloadedStrings #-}

module Sensei.UserSpec where

import Data.Aeson
import Data.Time.LocalTime
import Sensei.API
import Sensei.TestHelper
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import qualified Data.Map as Map

spec :: Spec
spec = describe "Users Management" $ do

  describe "User Profile" $ do

    it "can deserialize version 0 JSON" $ do
      let jsonProfile = "{\"userStartOfDay\":\"08:00:00\",\"userEndOfDay\":\"18:30:00\",\"userName\":\"arnaud\", \"userTimezone\":\"+01:00\"}"
      eitherDecode jsonProfile `shouldBe`
        Right defaultProfile { userFlowTypes = Nothing }

    it "can deserialize version 1 JSON" $ do
      let jsonProfile = "{\"userStartOfDay\":\"08:00:00\",\"userEndOfDay\":\"18:30:00\",\"userName\":\"arnaud\",\"userProfileVersion\":1, \"userTimezone\":\"+01:00\",\"userFlowTypes\":[\"Experimenting\"]}"
      eitherDecode jsonProfile `shouldBe`
        Right defaultProfile { userFlowTypes =  Just (Map.fromList [(FlowType "Experimenting", "#010aab")]) }

    it "can deserialize version 2 JSON" $ do
      let jsonProfile = "{\"userStartOfDay\":\"08:00:00\",\"userProfileVersion\":2,\"userEndOfDay\":\"18:30:00\",\"userName\":\"arnaud\",\"userTimezone\":\"+01:00\",\"userFlowTypes\":[[\"Experimenting\",\"#0022dd\"]]}"
      eitherDecode jsonProfile `shouldBe`
        Right defaultProfile { userFlowTypes =  Just (Map.fromList [(FlowType "Experimenting", "#0022dd")]) }

  withApp app $
    describe "Users API" $ do
      it "GET /users/<user> returns defualt profile" $ do
        getJSON "/users/arnaud"
          `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode defaultProfile)

      it "PUT /users/<user> sets user profile" $ do
        let profile = UserProfile {userName = "robert", userTimezone = hoursToTimeZone 1, userStartOfDay = TimeOfDay 08 00 00, userEndOfDay = TimeOfDay 18 30 00, userFlowTypes = Nothing }
        putJSON_ "/users/arnaud" profile

        getJSON "/users/arnaud"
          `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode profile)
