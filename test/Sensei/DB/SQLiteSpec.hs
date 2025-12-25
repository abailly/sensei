{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Sensei.DB.SQLiteSpec where

import Control.Exception.Safe (throwIO)
import Control.Monad.Reader
import qualified Data.ByteString
import Data.Functor (void)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Database.SQLite.Simple as SQLite
import Preface.Log
import Preface.Utils (toText)
import Sensei.API
import Sensei.DB as DB
import qualified Sensei.DB.File as File
import Sensei.DB.Log ()
import Sensei.DB.Model (canReadFlowsAndTracesWritten)
import Sensei.DB.SQLite (SQLiteDBError (..), runDB, withBackup)
import Sensei.TestHelper
import System.Directory
import System.FilePath (takeDirectory, takeFileName)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "SQLite DB" $ do
  around withTempFile $
    describe "Backup files" $ do
      let isBackupFileFor file fp =
            file `isPrefixOf` takeFileName fp
              && ".bak." `isInfixOf` takeFileName fp

      it "backup file before running action" $ \tmp -> do
        writeFile tmp "Foo"

        withBackup tmp $ \file -> do
          writeFile file "Bar"

          let dir = takeDirectory file
          backups <- filter (isBackupFileFor tmp) <$> listDirectory dir

          backups `shouldNotBe` []
          readFile (head backups) `shouldReturn` "Foo"

      it "Delete backup file after action completes successfuly" $ \tmp -> do
        withBackup tmp $ \file -> do
          writeFile file "Bar"

        let dir = takeDirectory tmp

        files <- listDirectory dir
        filter (isBackupFileFor tmp) files `shouldBe` []

      it "Restore initial file from backup and delete backup after action throws SQLiteDBError" $ \tmp -> do
        writeFile tmp "Foo"

        withBackup
          tmp
          ( \file -> do
              writeFile file "Bar"

              void $ throwIO $ SQLiteDBError "select * from foo;" "some error"
          )
          `shouldThrow` \(_ :: SQLiteDBError) -> True

        let dir = takeDirectory tmp

        files <- listDirectory dir
        filter (isBackupFileFor tmp) files `shouldBe` []
        readFile tmp `shouldReturn` "Foo"

      it "Do not remove backup file after action throws IOException" $ \tmp -> do
        writeFile tmp "Foo"

        withBackup
          tmp
          ( \file -> do
              writeFile file "Bar"

              void $ throwIO $ userError "some error"
          )
          `shouldThrow` anyIOException

        let dir = takeDirectory tmp

        files <- listDirectory dir
        filter (isBackupFileFor tmp) files `shouldNotBe` []
        mapM_ removeFile (filter (isBackupFileFor tmp) files)

  around withTempFile $
    describe "Basic Operations" $ do
      it "matches DB model" $ \tempdb ->
        property $ withMaxSuccess 400 $ canReadFlowsAndTracesWritten tempdb (runDB tempdb "." fakeLogger)

      it "gets IO-based current time when time is not set" $ \tempdb -> do
        res <- runDB tempdb "." fakeLogger $ do
          initLogStorage
          t1 <- DB.getCurrentTime defaultProfile
          t2 <- DB.getCurrentTime defaultProfile
          pure (t1, t2)

        uncurry (<) res `shouldBe` True

      it "throws SQLiteDBError when querying uninitialised DB" $ \tempdb -> do
        removePathForcibly tempdb

        runDB tempdb "." fakeLogger (DB.getCurrentTime defaultProfile)
          `shouldThrow` \SQLiteDBError {} -> True

      it "logs all DB operations using given logger" $ \tempdb -> do
        let time1 = UTCTime (toEnum 50000) 0

        res <- withLogger "test" $ \logger -> do
          runDB tempdb "." logger $
            runReaderT
              ( do
                  initLogStorage
                  setCurrentTime defaultProfile time1
                  DB.getCurrentTime defaultProfile
              )
              logger

        -- TODO this test has nothing to do with logger
        res `shouldBe` time1

      it "gets latest current time when time is set explicitly" $ \tempdb -> do
        let time1 = UTCTime (toEnum 50000) 0
            time2 = UTCTime (toEnum 50000) 100

        res <- runDB tempdb "." fakeLogger $ do
          initLogStorage
          setCurrentTime defaultProfile time1
          setCurrentTime defaultProfile time2
          DB.getCurrentTime defaultProfile

        res `shouldBe` time2

      it "retrieves user by its ID" $ \tempdb -> do
        (uid, profile) <- runDB tempdb "." fakeLogger $ do
          initLogStorage
          uid <- insertProfile defaultProfile
          (uid,) <$> readProfileById uid

        profile `shouldBe` defaultProfile {userId = uid}

      it "always retrieve profile with UID set" $ \tempdb -> do
        (uid, UserProfile {userId}) <- runDB tempdb "." fakeLogger $ do
          initLogStorage
          uid <- insertProfile defaultProfile
          writeProfile defaultProfile
          (uid,) <$> readProfile (userName defaultProfile)

        userId `shouldBe` uid

      it "indexes newly inserted note on the fly" $ \tempdb -> do
        let noteTime = UTCTime (toEnum 50000) 1000
            content = "foo bar baz cat"
            note1 = NoteFlow "arnaud" noteTime "some/dir" content
        res <- runDB tempdb "." fakeLogger $ do
          initLogStorage
          writeEvent (EventNote note1)
          searchNotes defaultProfile "foo"
        res
          `shouldBe` [ NoteView
                         { noteStart = utcToLocalTimeTZ (tzByLabel $ userTimezone defaultProfile) noteTime,
                           noteView = content,
                           noteProject = "dir",
                           noteTags = []
                         }
                     ]

      it "stores article content in separate table and replaces with hash in event log" $ \tempdb -> do
        let articleTime = UTCTime (toEnum 50000) 2000
            articleContent = "# Test Article\n\nThis is a test article with some content."
            articleOp =
              PublishArticle
                { _articleUser = "arnaud",
                  _articleTimestamp = articleTime,
                  _articleDir = "articles",
                  _article = articleContent,
                  _articleDate = Nothing
                }

        runDB tempdb "." fakeLogger $ do
          initLogStorage
          writeEvent (EventArticle articleOp)

        -- Verify the event log contains the hash, not the content
        eventLogRows <- SQLite.withConnection tempdb $ \cnx ->
          SQLite.query_ cnx "select flow_data from event_log where flow_type = 'Article';" :: IO [[Data.Text.Text]]

        let eventData = head (head eventLogRows)
        (not $ articleContent `Data.Text.isInfixOf` eventData) `shouldBe` True
        ("\"article\":" `Data.Text.isInfixOf` eventData) `shouldBe` True

        -- Verify the articles table contains the content
        articleRows <- SQLite.withConnection tempdb $ \cnx ->
          SQLite.query_ cnx "select hash, content from articles;" :: IO [(Data.Text.Text, Data.ByteString.ByteString)]

        length articleRows `shouldBe` 1
        let (hash, storedContentBytes) = head articleRows
            storedContent = Data.Text.Encoding.decodeUtf8 storedContentBytes
        storedContent `shouldBe` articleContent
        -- Verify the hash is hex-encoded (only contains hex chars)
        Data.Text.all (\c -> c `elem` ("0123456789abcdef" :: String)) hash `shouldBe` True

  around withTempFile $
    describe "Migrations" $ do
      it "populate user column from flow_data" $ \tmp -> do
        copyFile "test.sqlite" tmp

        runDB tmp "." fakeLogger initLogStorage

        res <- SQLite.withConnection tmp $ \cnx ->
          SQLite.query_ cnx "select user from event_log;"

        head res `shouldBe` ["arnaud" :: String]

      it "adds existing file-based user profile to DB with uid" $ \tmp ->
        withTempDir $ \dir -> do
          void $ File.writeProfileFile defaultProfile dir

          runDB tmp dir fakeLogger initLogStorage

          rows <- SQLite.withConnection tmp $ \cnx ->
            SQLite.query_ cnx "select id, user from users;"

          rows `shouldBe` [(1 :: Int, "arnaud" :: String)]

      it "do not add existing file-based user profile to DB with uid given user exists in DB" $ \tmp ->
        withTempDir $ \dir -> do
          uid <- runDB tmp dir fakeLogger $ do
            initLogStorage
            insertProfile defaultProfile

          void $ File.writeProfileFile defaultProfile dir

          runDB tmp dir fakeLogger initLogStorage

          rows <- SQLite.withConnection tmp $ \cnx ->
            SQLite.query_ cnx "select id, uid, user from users;"

          rows `shouldBe` [(1 :: Int, toText uid, "arnaud" :: String)]
