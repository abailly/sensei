{-# LANGUAGE OverloadedStrings #-}
module Sensei.FlowAPISpec where

import Test.Hspec
import Test.Hspec.Wai
import Sensei.API
import Sensei.TestHelper
import Data.Time

spec :: Spec
spec =
  withApp $ describe "Flows API" $ do

  it "POST /flows/<user>/Other with Flow body register start of a flow event" $ do
    let flow = FlowState "arnaud" (UTCTime (toEnum 5000) 0) "some/directory"
    postJSON "/flows/arnaud/Other" flow
      `shouldRespondWith` 200
