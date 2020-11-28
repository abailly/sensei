{-# LANGUAGE OverloadedStrings #-}

module Sensei.ServerSpec where

import Sensei.TestHelper
import Test.Hspec
import Sensei.Server.Config
import Test.Hspec.Wai

spec :: Spec
spec =
  describe "Sensei Server" $ do
    withApp (withoutStorage app) $ it "creates storage file if it does not exist" $ do
      getJSON "/flows/arnaud"
        `shouldRespondWith` 200

    withApp (app { withEnv = Prod}) $ it "serves 'index.html' embedded" $ do
      get "/index.html"
        `shouldRespondWith` 200
