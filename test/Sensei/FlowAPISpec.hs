{-# LANGUAGE OverloadedStrings #-}

module Sensei.FlowAPISpec where

import Data.Aeson
import Data.Time
import Sensei.API
import Sensei.TestHelper
import Test.Hspec
import Data.Time
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher

spec :: Spec
spec = withApp $ describe "Flows API" $ do

  it "POST /flows/<user>/Other with Flow body register start of a flow event" $ do
    let flow = FlowState "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
    postJSON "/flows/arnaud/Other" flow
      `shouldRespondWith` 200

  it "GET /flows/<user> retrieves all Flows ungrouped" $ do
    let flow1 = FlowState "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
        flow2 = FlowState "arnaud" (UTCTime (toEnum 50001) 0) "some/directory"
    postJSON "/flows/arnaud/Other" flow1
    postJSON "/flows/arnaud/Meeting" flow2

    let expectedGroups =
          [ Leaf
              [ FlowView (LocalTime (toEnum 50000) (TimeOfDay 1 0 0)) (LocalTime (toEnum 50000) (TimeOfDay 18 30 0)) Other,
                FlowView (LocalTime (toEnum 50001) (TimeOfDay 1 0 0)) (LocalTime (toEnum 50001) (TimeOfDay 1 0 0)) Meeting
              ]
          ]

    getJSON "/flows/arnaud"
      `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode expectedGroups)

  it "GET /flows/<user>?group=Day retrieves all Flows grouped by Day" $ do
    let flow1 = FlowState "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
        flow2 = FlowState "arnaud" (UTCTime (toEnum 50001) 0) "some/directory"
    postJSON "/flows/arnaud/Other" flow1
    postJSON "/flows/arnaud/Meeting" flow2

    let expectedGroups =
          [ GroupLevel Day (LocalTime (toEnum 50000) (TimeOfDay 1 0 0))
            (Leaf [FlowView (LocalTime (toEnum 50000) (TimeOfDay 1 0 0)) (LocalTime (toEnum 50000) (TimeOfDay 18 30 0)) Other]),
            GroupLevel Day (LocalTime (toEnum 50001) (TimeOfDay 1 0 0))
            (Leaf [FlowView (LocalTime (toEnum 50001) (TimeOfDay 1 0 0)) (LocalTime (toEnum 50001) (TimeOfDay 18 30 0)) Meeting])
          ]

    getJSON "/flows/arnaud?group=Day"
      `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode expectedGroups)
