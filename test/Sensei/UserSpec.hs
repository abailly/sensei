{-# LANGUAGE OverloadedStrings #-}

module Sensei.UserSpec where

import Data.Aeson
import Data.Time.LocalTime
import Sensei.API
import Sensei.TestHelper
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher

spec :: Spec
spec =
  withApp app $
    describe "Users API" $ do
      it "GET /users/<user> returns defualt profile" $ do
        getJSON "/users/arnaud"
          `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode defaultProfile)

      it "PUT /users/<user> sets user profile" $ do
        let profile = UserProfile {userName = "robert", userTimezone = hoursToTimeZone 1, userStartOfDay = TimeOfDay 08 00 00, userEndOfDay = TimeOfDay 18 30 00, userFlowTypes = Nothing}
        putJSON_ "/users/arnaud" profile

        getJSON "/users/arnaud"
          `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode profile)
