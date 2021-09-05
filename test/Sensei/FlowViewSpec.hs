{-# LANGUAGE OverloadedStrings #-}

module Sensei.FlowViewSpec where

import Data.Time
import Sensei.API
import Test.Hspec
import Data.UUID(nil)

spec :: Spec
spec = describe "FlowViews Timings" $ do
  let tz = hoursToTimeZone 1
      at0800 = LocalTime (toEnum 5000) (TimeOfDay 08 0 0)
      at0900 = LocalTime (toEnum 5000) (TimeOfDay 09 0 0)
      at1000 = LocalTime (toEnum 5000) (TimeOfDay 10 0 0)
      at1100 = LocalTime (toEnum 5000) (TimeOfDay 11 0 0)
      at1200 = LocalTime (toEnum 5000) (TimeOfDay 12 0 0)
      at1300 = LocalTime (toEnum 5000) (TimeOfDay 13 0 0)
      at1400 = LocalTime (toEnum 5000) (TimeOfDay 14 0 0)
      at1700 = LocalTime (toEnum 5000) (TimeOfDay 17 0 0)
      at1800 = LocalTime (toEnum 5000) (TimeOfDay 18 0 0)
      endOfDay = TimeOfDay 18 00 00
      someView =
        FlowView
          { flowStart = at1300,
            flowEnd = at1300,
            viewType = Other,
            flowProject = "foo"
          }

  describe "appendFlow" $ do
    it "do not overwrite previous flowview end if different from start" $ do
      let flows =
            EventFlow
              <$> [ Flow Other "user" (UTCTime (toEnum 5000) (3600 * 12)) "foo",
                    Flow End "user" (UTCTime (toEnum 5000) (3600 * 11)) "foo",
                    Flow Other "user" (UTCTime (toEnum 5000) (3600 * 10)) "foo"
                  ]
          views = zipWith (flip EventView nil) [0..] flows
          
      foldr (appendFlow tz endOfDay mempty) [] views
        `shouldBe` [ someView,
                     someView
                       { flowStart = at1100,
                         flowEnd = at1200
                       }
                   ]

  describe "summarize" $ do
    it "add durations for views with same type" $ do
      let view1 = someView {flowStart = at1200}
          view2 = someView {flowEnd = at1400}
      summarize [view1, view2] `shouldBe` [(Other, 3600 * 2)]

  describe "flowEnd" $ do
    it "set flowView's end time to default end time given its duration is 0" $ do
      let endTime = LocalTime (toEnum 5000) (TimeOfDay 16 0 0)

      fillFlowEnd endOfDay someView endTime `shouldBe` someView {flowEnd = endTime}

    it
      "set flowView's end time to one hour after start time given its duration is 0, \
      \ it starts after end of day and \
      \ next flow is on other day"
      $ do
        let at1830 = LocalTime (toEnum 5000) (TimeOfDay 18 30 0)
            view =
              someView
                { flowStart = at1830,
                  flowEnd = at1830
                }
            endTime = LocalTime (toEnum 5001) (TimeOfDay 16 0 0)

        fillFlowEnd endOfDay view endTime `shouldBe` view {flowEnd = LocalTime (toEnum 5000) (TimeOfDay 19 30 0)}

  describe "normalizeViewsForDay" $ do
    let startTime = at0800
        endTime = at1800

    it "extend last view when it's not ended" $ do
      let view =
            FlowView
              { flowStart = startTime,
                flowEnd = startTime,
                viewType = FlowType "Meeting",
                flowProject = "foo"
              }

      normalizeViewsForDay startTime endTime [view]
        `shouldBe` [view {flowEnd = endTime}]

    it "append Other view at end of last view given it does not end the day" $ do
      let view =
            FlowView
              { flowStart = startTime,
                flowEnd = at1700,
                viewType = (FlowType "Meeting"),
                flowProject = "foo"
              }

      normalizeViewsForDay startTime endTime [view]
        `shouldBe` [ view,
                     FlowView {flowStart = at1700, flowEnd = at1800, viewType = Other, flowProject = "foo"}
                   ]

    it "extending last view preserves other views" $ do
      let view1 =
            FlowView
              { flowStart = at0800,
                flowEnd = at0900,
                viewType = (FlowType "Learning"),
                flowProject = "foo"
              }
          view2 =
            FlowView
              { flowStart = at1000,
                flowEnd = at1000,
                viewType = (FlowType "Meeting"),
                flowProject = "bar"
              }

      normalizeViewsForDay startTime endTime [view1, view2]
        `shouldBe` [view1, view2 {flowEnd = at1800}]
