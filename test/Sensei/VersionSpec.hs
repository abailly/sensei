{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.VersionSpec where

import Sensei.Version
import Servant
import Test.Hspec
import Test.Hspec.Wai

type TestAPI = CheckVersion "1.2.3" :> Get '[JSON] Int

testApp :: IO Application
testApp = pure $ serve (Proxy @TestAPI) (pure 12)

spec :: Spec
spec = with testApp $
  describe "CheckVersion combinator" $ do
    it "rejects request with 406 given x-api-version header is not set" $ do
      get "/" `shouldRespondWith` 406
    it "rejects request with 406 given x-api-version header has different minor version" $ do
      request "GET" "/" [("X-api-version", "1.3.0")] mempty `shouldRespondWith` 406
    it "accepts request given x-api-version header has different same version" $ do
      request "GET" "/" [("X-api-version", "1.2.4")] mempty `shouldRespondWith` 200
