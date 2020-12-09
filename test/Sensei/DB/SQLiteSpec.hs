module Sensei.DB.SQLiteSpec where

import Sensei.API
import Sensei.DB
import qualified Sensei.DB.File as File
import Sensei.DB.Model (canReadFlowsAndTracesWritten)
import qualified Sensei.DB.Model as Model
import Sensei.DB.SQLite (migrateFileDB, runDB)
import Sensei.TestHelper
import System.FilePath((<.>))
import Data.Functor(void)
import Data.Time
import Data.Text(Text)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  around withTempFile $
    describe "SQLite DB" $ do
      it "matches DB model" $ \tempdb -> property $ canReadFlowsAndTracesWritten (runDB tempdb ".")

      it "can migrate a File-based log to SQLite-based one" $ \tempdb -> do
        let checks :: DB db => db ([FlowView], [(LocalTime, Text)], [CommandView])
            checks = (,,)
              <$> readViews defaultProfile
              <*> readNotes defaultProfile
              <*> readCommands defaultProfile
        actions <- generate arbitrary
        void $ File.runDB tempdb "." $ initLogStorage >> (Model.runActions actions)
        expected <- File.runDB tempdb "." checks

        res <- migrateFileDB tempdb "."
        actual <- runDB tempdb "." checks

        res `shouldBe` Right (tempdb <.> "old")
        actual `shouldBe` expected
