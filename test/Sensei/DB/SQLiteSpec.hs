module Sensei.DB.SQLiteSpec where

import Sensei.DB.Model (canReadFlowsAndTracesWritten)
import Sensei.DB.SQLite (runDB)
import Sensei.TestHelper
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  around withTempFile $
    describe "SQLite DB" $ do
      it "matches DB model" $ \tempdb -> property $ canReadFlowsAndTracesWritten (runDB tempdb ".")
