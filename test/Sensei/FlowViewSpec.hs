{-# LANGUAGE OverloadedStrings #-}
module Sensei.FlowViewSpec where

import Sensei.API
import Test.Hspec
import Data.Time

spec :: Spec
spec = describe "FlowViews Timings" $ do
  let tz = hoursToTimeZone 1
      endOfDay = TimeOfDay 18 00 00

  describe "appendFlow" $ do
    it "do not overwrite previous flowview end if different from start" $ do
      let flows = [
                    Flow Other (FlowState "user" (UTCTime (toEnum 5000) (3600 * 12)) "foo") 1,
                    Flow End (FlowState "user" (UTCTime (toEnum 5000) (3600 * 11)) "foo") 1,
                    Flow Other (FlowState "user" (UTCTime (toEnum 5000) (3600 * 10)) "foo") 1
                  ]

      foldr (appendFlow tz endOfDay) [] flows
        `shouldBe` [ FlowView (LocalTime (toEnum 5000) (TimeOfDay 13 0 0)) (LocalTime (toEnum 5000) (TimeOfDay 13 0 0)) Other,
                     FlowView (LocalTime (toEnum 5000) (TimeOfDay 11 0 0)) (LocalTime (toEnum 5000) (TimeOfDay 12 0 0)) Other
                   ]

  describe "summarize" $ do
    it "add durations for views with same type" $ do
      let view1 = FlowView (LocalTime (toEnum 5000) (TimeOfDay 12 0 0)) (LocalTime (toEnum 5000)  (TimeOfDay 13 0 0)) Other
          view2 = FlowView (LocalTime (toEnum 5000)  (TimeOfDay 13 0 0)) (LocalTime (toEnum 5000)  (TimeOfDay 14 0 0)) Other
      summarize [view1, view2] `shouldBe` [(Other, 3600 * 2)]

  describe "flowEnd" $ do
    it "set flowView's end time to default end time given its duration is 0" $ do
      let view = FlowView (LocalTime (toEnum 5000)  (TimeOfDay 12 0 0)) (LocalTime (toEnum 5000)  (TimeOfDay 12 0 0)) Other
          endTime = LocalTime (toEnum 5000)  (TimeOfDay 16 0 0)

      fillFlowEnd endOfDay view endTime `shouldBe` view {flowEnd = endTime}

    it "set flowView's end time to one hour after start time given its duration is 0, it starts after end of day and next flow is on other day" $ do
      let view = FlowView (LocalTime (toEnum 5000)  (TimeOfDay 18 30 0)) (LocalTime (toEnum 5000)  (TimeOfDay 18 30 0)) Other
          endTime = LocalTime (toEnum 5001)  (TimeOfDay 16 0 0)

      fillFlowEnd endOfDay view endTime `shouldBe` view {flowEnd = LocalTime (toEnum 5000)  (TimeOfDay 19 30 0)}

  describe "normalizeViewsForDay" $ do
    it "extend last view when it's not ended" $ do
      let view = FlowView (LocalTime (toEnum 5000)  (TimeOfDay 8 0 0)) (LocalTime (toEnum 5000)  (TimeOfDay 8 0 0)) (FlowType "Meeting")
          startTime = LocalTime (toEnum 5000)  (TimeOfDay 8 0 0)
          endTime = LocalTime (toEnum 5000)  (TimeOfDay 18 0 0)

      normalizeViewsForDay startTime endTime [view] `shouldBe`
        [FlowView (LocalTime (toEnum 5000)  (TimeOfDay 8 0 0)) (LocalTime (toEnum 5000)  (TimeOfDay 18 0 0)) (FlowType "Meeting")]

    it "append Other view at end of last view given it does not end the day" $ do
      let view = FlowView (LocalTime (toEnum 5000)  (TimeOfDay 8 0 0)) (LocalTime (toEnum 5000)  (TimeOfDay 17 0 0)) (FlowType "Meeting")
          startTime = LocalTime (toEnum 5000)  (TimeOfDay 8 0 0)
          endTime = LocalTime (toEnum 5000)  (TimeOfDay 18 0 0)

      normalizeViewsForDay startTime endTime [view] `shouldBe`
        [FlowView (LocalTime (toEnum 5000)  (TimeOfDay 8 0 0)) (LocalTime (toEnum 5000)  (TimeOfDay 17 0 0)) (FlowType "Meeting"),
         FlowView (LocalTime (toEnum 5000)  (TimeOfDay 17 0 0)) (LocalTime (toEnum 5000)  (TimeOfDay 18 0 0)) Other ]

    it "extending last view preserves other views" $ do
      let view1 = FlowView (LocalTime (toEnum 5000)  (TimeOfDay 8 0 0)) (LocalTime (toEnum 5000)  (TimeOfDay 9 0 0)) (FlowType "Learning")
          view2 = FlowView (LocalTime (toEnum 5000)  (TimeOfDay 10 0 0)) (LocalTime (toEnum 5000)  (TimeOfDay 10 0 0)) (FlowType "Meeting")
          startTime = LocalTime (toEnum 5000)  (TimeOfDay 8 0 0)
          endTime = LocalTime (toEnum 5000)  (TimeOfDay 18 0 0)

      normalizeViewsForDay startTime endTime [view1, view2] `shouldBe`
        [view1, FlowView (LocalTime (toEnum 5000)  (TimeOfDay 10 0 0)) (LocalTime (toEnum 5000)  (TimeOfDay 18 0 0)) (FlowType "Meeting")]
