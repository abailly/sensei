{-# LANGUAGE OverloadedStrings #-}
module Sensei.FlowAPISpec where

import Test.Hspec
import Data.Aeson
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import Sensei.API
import Sensei.TestHelper
import System.Posix.Temp (mkstemp)
import Data.Time

spec :: Spec
spec =
  with mkApp $ describe "Flows API" $ do

  it "POST /flows/<user>/Other with Flow body register start of a flow event" $ do
    let flow = FlowState "arnaud" (UTCTime (toEnum 5000) 0) "some/directory"
    postJSON "/flows/arnaud/Other" flow
      `shouldRespondWith` 200
