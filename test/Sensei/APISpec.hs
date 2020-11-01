module Sensei.APISpec where

import Sensei.API
import Test.Hspec
import Data.Time.Clock

spec :: Spec
spec = describe "FlowViews Timings" $ do
  describe "summarize" $ do
    it "add durations for views with same type" $ do
      let view1 = FlowView (UTCTime (toEnum 5000) 0) (UTCTime (toEnum 5000) 1000) Other
          view2 = FlowView (UTCTime (toEnum 5000) 1000) (UTCTime (toEnum 5000) 2000) Other
      summarize [view1, view2] `shouldBe` [(Other, 2000)]

  describe "flowEnd" $ do
    it "set flowView's end time to given time given they are on the same day" $ do
      let view = FlowView (UTCTime (toEnum 5000) 0) (UTCTime (toEnum 5000) 1000) Other
          endTime = UTCTime (toEnum 5000) 3000

      fillFlowEnd view endTime `shouldBe` view {flowEnd = endTime}
