module Sensei.APISpec where

import Sensei.API
import Test.Hspec
import Data.Time.Clock

spec :: Spec
spec = describe "FlowViews Timings" $ do
  describe "flowEnd" $ do
    it "set flowView's end time to given time given they are on the same day" $ do
      let view = FlowView (UTCTime (toEnum 5000) 0) (UTCTime (toEnum 5000) 1000) 1000 Other
          endTime = UTCTime (toEnum 5000) 3000

      fillFlowEnd view endTime `shouldBe` view {flowEnd = endTime, duration = 3000}
