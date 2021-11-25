{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.GoalAPISpec where

import Sensei.API
import Sensei.Builder (aDay)
import Sensei.TestHelper
import Sensei.Time (UTCTime (..))
import Test.Hspec

spec :: Spec
spec = withApp app $ do
  it "POST /api/goals/<user> accepts a single Goal operation" $ do
    let op =
          GoalOp
            { _goalOp = goal "some goal",
              _goalUser = "arnaud",
              _goalTimestamp = UTCTime aDay 0,
              _goalDir = "some/directory"
            }
    postJSON "/api/goals/arnaud" op
      `shouldRespondWith` 200

  it "GET /api/goals/<user> returns complete goal graph" $ do
    let g1 =
          GoalOp
            { _goalOp = goal "some goal",
              _goalUser = "arnaud",
              _goalTimestamp = UTCTime aDay 0,
              _goalDir = "some/directory"
            }
        g2 =
          GoalOp
            { _goalOp = goal "other goal",
              _goalUser = "arnaud",
              _goalTimestamp = UTCTime aDay 0,
              _goalDir = "some/directory"
            }
    postJSON_ "/api/goals/arnaud" g1
    postJSON_ "/api/goals/arnaud" g2
    getJSON "/api/goals/arnaud"
      `shouldRespondJSONBody` Goals {goalsGraph = [("other goal", ["some goal"]), ("some goal", [])]}
