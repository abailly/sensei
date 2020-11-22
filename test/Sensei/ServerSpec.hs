{-# LANGUAGE OverloadedStrings #-}

module Sensei.ServerSpec where

import Sensei.TestHelper
import Test.Hspec
import Test.Hspec.Wai

spec :: Spec
spec =
  describe "Sensei Server" $ do
    withApp (withoutStorage app) $ it "creates storage file if it does not exist" $ do
      getJSON "/flows/arnaud"
        `shouldRespondWith` 200
