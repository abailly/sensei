module Sensei.DB.FileSpec where

import Sensei.DB.Model (canReadFlowsAndTracesWritten)
import Sensei.DB.File (runDB)
import Sensei.TestHelper
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  around withTempFile $
    describe "File DB" $ do
      it "matches DB model" $ \tempdb -> property $ canReadFlowsAndTracesWritten (runDB tempdb ".")
