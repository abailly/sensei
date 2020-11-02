{-# LANGUAGE OverloadedStrings #-}
module Sensei.UserSpec where

import Test.Hspec
import Data.Aeson
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import Sensei.API
import Sensei.TestHelper
import System.Posix.Temp (mkstemp)
import Data.Time.LocalTime

spec :: Spec
spec =
  with mkApp $ describe "Users API" $ do

  it "GET /users/<user> retrieves user profile" $ do
    let defaultProfile = UserProfile { userName = "arnaud", userTimezone = hoursToTimeZone 1, userStartOfDay = TimeOfDay 08 00 00 , userEndOfDay = TimeOfDay 18 30 00  }
    getJSON "/users/arnaud"
      `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode defaultProfile)
