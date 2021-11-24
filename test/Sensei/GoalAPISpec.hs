{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.GoalAPISpec where

import Sensei.API
import Sensei.TestHelper
import Test.Hspec

spec :: Spec
spec = withApp app $ do
  it "POST /api/goals/<user> accepts a single Goal operation" $ do
    let op = GoalOp {goalOp = goal "some goal"}
    postJSON "/api/goals/arnaud" op
      `shouldRespondWith` 200

  it "GET /api/goals/<user> returns complete goal graph" $ do
    let g1 = GoalOp {goalOp = goal "some goal"}
        g2 = GoalOp {goalOp = goal "other goal"}
    postJSON_ "/api/goals/arnaud" g1
    postJSON_ "/api/goals/arnaud" g2
    getJSON "/api/goals/arnaud"
      `shouldRespondJSONBody` Goals { goalsGraph = [("other goal", ["some goal"])] }
