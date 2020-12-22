{-# LANGUAGE OverloadedStrings #-}
module Sensei.DB.SQLiteSpec where

import Data.Functor (void)
import Data.Text (Text)
import Sensei.API
import Sensei.DB
import qualified Sensei.DB.File as File
import Sensei.DB.Model (canReadFlowsAndTracesWritten)
import qualified Sensei.DB.Model as Model
import Sensei.DB.SQLite (migrateFileDB, runDB)
import Sensei.TestHelper
import Sensei.Time hiding (getCurrentTime)
import System.FilePath ((<.>))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  around withTempFile $
    describe "SQLite DB" $ do
      it "matches DB model" $ \tempdb -> property $ canReadFlowsAndTracesWritten (runDB tempdb ".")

      it "gets IO-based current time when time is not set" $ \tempdb -> property $ do
        res <- runDB tempdb "." $ do
          initLogStorage
          t1 <- getCurrentTime defaultProfile
          t2 <- getCurrentTime defaultProfile
          pure (t1, t2)

        uncurry (<) res `shouldBe` True

      it "gets latest current time when time is set explicitly" $ \tempdb -> property $ do
        let time1 = UTCTime (toEnum 50000) 0
            time2 = UTCTime (toEnum 50000) 100

        res <- runDB tempdb "." $ do
          initLogStorage
          setCurrentTime defaultProfile time1
          setCurrentTime defaultProfile time2
          getCurrentTime defaultProfile

        res `shouldBe` time2

      it "can migrate a File-based log to SQLite-based one" $ \tempdb -> do
        let checks :: DB db => db ([FlowView], [(LocalTime, Text)], [CommandView])
            checks =
              (,,)
                <$> readViews defaultProfile
                <*> readNotes defaultProfile (TimeRange Model.startTime (addUTCTime 1000000 Model.startTime))
                <*> readCommands defaultProfile
        actions <- generate $ resize 100 arbitrary
        void $ File.runDB tempdb "." $ initLogStorage >> (Model.runActions actions)
        expected <- File.runDB tempdb "." checks

        res <- migrateFileDB tempdb "."

        res `shouldBe` Right (tempdb <.> "old")
        actual <- runDB tempdb "." checks
        actual `shouldBe` expected

      it "indexes newly inserted note on the fly" $ \tempdb -> do
        let noteTime = (UTCTime (toEnum 50000) 1000)
            content = "foo bar baz cat"
            note1 = NoteFlow "arnaud" noteTime "some/dir" content
        res <- runDB tempdb "." $ do
          initLogStorage
          writeFlow (EventNote note1)
          searchNotes defaultProfile "foo"

        res `shouldBe` [(utcToLocalTime (userTimezone defaultProfile) noteTime, content)]
