
module Sensei.ServerSpec where

import Data.Either (isLeft)
import Sensei.Builder (anOtherFlow, postFlow)
import Sensei.Server
import Sensei.TestHelper
import Sensei.Time
import Test.Hspec
import Test.Hspec.Wai

spec :: Spec
spec =
  describe "Sensei Server" $ do
    withApp (withoutStorage app) $
      it "creates storage file if it does not exist" $ do
        getJSON "/api/flows/arnaud"
          `shouldRespondWith` 200

    withApp (app{withEnv = Prod}) $
      it "serves 'index.html' embedded" $ do
        get "/index.html"
          `shouldRespondWith` 200

    withApp app $
      it "PUT /time/<user> sets current time for user" $ do
        let times = Timestamp $ UTCTime (toEnum 50000) 0

        putJSON_ "/time/arnaud" times

        getJSON "/time/arnaud"
          `shouldRespondJSONBody` times

    withApp (app{withFailingStorage = True}) $
      it "returns error 400 with details given DB fails to access storage file" $ do
        postFlow anOtherFlow
          `shouldRespondWith` 500

    describe "Run mode and Options" $ do
      it "parses client mode and pass arguments to it" $ do
        runOptionsParser ["client", "foo", "--", "-b", "ar"]
          `shouldBe` Right (ClientOptions ["foo", "-b", "ar"])

      it "parses server mode and pass arguments to it" $ do
        runOptionsParser ["server", "foo", "--", "-b", "ar"]
          `shouldBe` Right (ServerOptions ["foo", "-b", "ar"])

      it "returns error message given unknown command" $ do
        runOptionsParser ["frobnicate"]
          `shouldSatisfy` isLeft

      it "returns error message given unescaped parseable arguments" $ do
        runOptionsParser ["client", "-x"]
          `shouldSatisfy` isLeft
