{-# LANGUAGE OverloadedStrings #-}

module Sensei.DB.SQLiteSpec where

import Data.Text (Text, unpack)
import Control.Monad.Reader
import Preface.Log
import Sensei.API
import Sensei.DB
import qualified Sensei.DB.File as File
import Sensei.DB.Log ()
import Sensei.DB.Model (canReadFlowsAndTracesWritten)
import qualified Sensei.DB.Model as Model
import Sensei.DB.SQLite (SQLiteDBError (..), migrateFileDB, runDB)
import Sensei.TestHelper
import Sensei.Time hiding (getCurrentTime)
import System.Directory
import System.FilePath ((<.>))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  around withTempFile $
    describe "SQLite DB" $ do
      it "matches DB model" $ \tempdb -> property $ canReadFlowsAndTracesWritten (runDB tempdb "." fakeLogger)

      it "gets IO-based current time when time is not set" $ \tempdb -> do
        res <- runDB tempdb "." fakeLogger $ do
          initLogStorage
          t1 <- getCurrentTime defaultProfile
          t2 <- getCurrentTime defaultProfile
          pure (t1, t2)

        uncurry (<) res `shouldBe` True

      it "throws SQLiteDBError when querying uninitialised DB" $ \tempdb -> do
        removePathForcibly tempdb

        (runDB tempdb "." fakeLogger $ getCurrentTime defaultProfile)
          `shouldThrow` \SQLiteDBError {} -> True

      it "logs all DB operations using given logger" $ \tempdb -> do
        let time1 = UTCTime (toEnum 50000) 0
        logger <- newLog "test"

        res <-
          runDB tempdb "." logger $
          runReaderT
            ( do
                initLogStorage
                writeProfile defaultProfile
                prof <- either (error.unpack) id <$> readProfile
                setCurrentTime prof time1
                getCurrentTime prof
            )
            logger

        res `shouldBe` time1

      it "gets latest current time when time is set explicitly" $ \tempdb -> do
        let time1 = UTCTime (toEnum 50000) 0
            time2 = UTCTime (toEnum 50000) 100

        res <- runDB tempdb "." fakeLogger $ do
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
        actual <- runDB tempdb "." fakeLogger checks
        actual `shouldBe` expected

      it "indexes newly inserted note on the fly" $ \tempdb -> do
        let noteTime = (UTCTime (toEnum 50000) 1000)
            content = "foo bar baz cat"
            note1 = NoteFlow "arnaud" noteTime "some/dir" content
        res <- runDB tempdb "." fakeLogger $ do
          initLogStorage
          writeEvent (EventNote note1)
          searchNotes defaultProfile "foo"

        res `shouldBe` [(utcToLocalTime (userTimezone defaultProfile) noteTime, content)]
