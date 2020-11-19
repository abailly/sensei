{-# LANGUAGE OverloadedStrings #-}
module Sensei.UserSpec where

import Test.Hspec
import Data.Aeson
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import Sensei.API
import Sensei.TestHelper
import Data.Time.LocalTime

spec :: Spec
spec =
  withApp $ describe "Users API" $ do

  it "PUT /users/<user> sets user profile" $ do
    let profile = UserProfile { userName = "robert", userTimezone = hoursToTimeZone 1, userStartOfDay = TimeOfDay 08 00 00 , userEndOfDay = TimeOfDay 18 30 00  }
    putJSON_ "/users/arnaud" profile

    getJSON "/users/arnaud"
      `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode profile)
