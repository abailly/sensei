{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.GoalAPISpec where

import Sensei.API
import Sensei.TestHelper
import Test.Hspec

spec :: Spec
spec = withApp app $ do
  it "POST /api/goals accepts a single Goal operation" $ do
    let op = GoalOp {goalOp = goal "some goal"}
    postJSON "/api/goals/arnaud" op
      `shouldRespondWith` 200
