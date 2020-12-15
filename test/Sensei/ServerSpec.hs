{-# LANGUAGE OverloadedStrings #-}

module Sensei.ServerSpec where

import Sensei.Server.Config
import Sensei.TestHelper
import Sensei.Time
import Test.Hspec
import Test.Hspec.Wai

spec :: Spec
spec =
  describe "Sensei Server" $ do
    withApp (withoutStorage app) $
      it "creates storage file if it does not exist" $ do
        getJSON "/flows/arnaud"
          `shouldRespondWith` 200

    withApp (app {withEnv = Prod}) $
      it "serves 'index.html' embedded" $ do
        get "/index.html"
          `shouldRespondWith` 200

    withApp app $
      it "PUT /time/<user> sets current time for user" $ do
        let times = Timestamp $ UTCTime (toEnum 50000) 0

        putJSON_ "/time/arnaud" times

        getJSON "/time/arnaud"
          `shouldRespondWith` ResponseMatcher 200 [] (jsonBodyEquals times)
