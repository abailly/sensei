module Sensei.DB.SQLiteSpec where

import Sensei.DB.Model
import Sensei.DB.SQLite
import Sensei.TestHelper
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  around withTempFile $
    describe "SQLite DB" $ do
      it "matches DB model" $ \tempdb -> property $ canReadFlowsAndTracesWritten (runSQLite tempdb)
