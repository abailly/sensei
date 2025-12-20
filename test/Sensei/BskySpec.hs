{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

module Sensei.BskySpec (spec) where

import Control.Exception (Exception, throwIO)
import Control.Exception.Safe (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT), ask, runReaderT)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Data (Proxy (..))
import Data.Either (isLeft)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Text (Text, isInfixOf, pack)
import Data.Time (UTCTime (..), addUTCTime)
import GHC.IO (unsafePerformIO)
import Network.URI.Extra (uriFromString)
import Preface.Log (LoggerEnv, fakeLogger)
import Sensei.API (Article (..), Event (EventNote), NoteFlow (..), UserProfile (..), defaultProfile)
import Sensei.Backend (Backend (..))
import Sensei.Backend.Class (BackendHandler (..), Backends)
import qualified Sensei.Backend.Class as Backend
import Sensei.Bsky
  ( BskyAuth (..),
    BskyNet (..),
    BskyPost,
    BskyRecord,
    BskySession (..),
    ListRecordsResponse (..),
    Record (..),
    bskyEventHandler,
    decodeAuthToken,
    publishArticle,
    record,
    text,
  )
import Sensei.Bsky.Core (BskyBackend (..), BskyLogin (..))
import Sensei.Builder (aDay, postNote, postNote_)
import Sensei.DB (DB (..))
import Sensei.Generators ()
import Sensei.Server (SerializedToken (..))
import Sensei.TestHelper (app, putJSON_, serializedSampleToken, withApp, withBackends, withDBRunner)
import Servant (JSON, mimeRender)
import Servant.Server (ServerError)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (HasCallStack, Spec, after_, before, describe, it, runIO, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Wai
import Test.QuickCheck (Property, arbitrary, forAll, (===))

spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @BskyBackend)

  let bskyBackend =
        BskyBackend
          { login =
              BskyLogin
                { identifier = "bob.bsky.social",
                  password = "password"
                },
            pdsUrl = fromJust $ uriFromString "https://some.social",
            userDID = "did:plc:test123456789012345",
            publicationId = "at://did:plc:s5wwr2ccjuqricdxiyiuspfv/pub.leaflet.publication/3m7zleg5tyc2b"
          }
      profileWithBsky =
        defaultProfile
          { backends =
              [ Backend bskyBackend
              ]
          }
      aBskyNote = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note #bsky"

  calls <- runIO (newIORef [])

  after_ (writeIORef calls []) $
    describe "POST /api/log with configured Bsky backend" $ do
      withApp (withBackends (mkBackends calls) app) $ it "propagate posted event to backend" $ do
        putJSON_ "/api/users/arnaud" profileWithBsky

        postNote_ aBskyNote

        liftIO $ (head <$> readIORef calls) `shouldReturn` EventNote aBskyNote

      withApp
        ( withBackends (mkBackends calls) $
            withDBRunner (dbFailsToWriteEvents profileWithBsky) app
        )
        $ it "does not propagate events to backend if DB write fails"
        $ do
          putJSON_ "/api/users/arnaud" profileWithBsky

          postNote aBskyNote `shouldRespondWith` 500

          liftIO $ (length <$> readIORef calls) `shouldReturn` 0

  describe "Bsky auth token" $ do
    prop "can deserialise base64-encoded auth token's claims" $ prop_deserialiseAuthToken
    prop "reject malformed tokens" $ prop_rejectMalformedTokens

  before (newBskyMockNet >>= \mockNet -> (mockNet,) <$> bskyEventHandler fakeLogger (bskyNet mockNet)) $
    describe "Bsky logic" $ do
      it "login with given credentials then post event with token" $ \(bskyMockNet, BackendHandler {handleEvent}) -> do
        handleEvent bskyBackend (EventNote aBskyNote)

        calledLogin bskyMockNet `shouldReturn` 1
        bskyMockNet `calledCreatePost` (1 :: Int)

      it "login only once when posting several events" $ \(bskyMockNet, BackendHandler {handleEvent}) -> do
        handleEvent bskyBackend (EventNote aBskyNote)
        handleEvent bskyBackend (EventNote aBskyNote)

        calledLogin bskyMockNet `shouldReturn` 1
        bskyMockNet `calledCreatePost` (2 :: Int)

      it "discards note if it does not contain #bsky tag" $ \(bskyMockNet, BackendHandler {handleEvent}) -> do
        let notForBsky = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note #foo"

        handleEvent bskyBackend (EventNote notForBsky)

        bskyMockNet `calledCreatePost` (0 :: Int)

      it "refreshes token given it has expired" $ \(bskyMockNet, BackendHandler {handleEvent}) -> do
        let accessJwt = defaultToken
            refreshJwt =
              unsafePerformIO $
                serializedSampleToken $
                  BskyAuth
                    { scope = "com.atproto.refresh",
                      sub = "did:plc:234567",
                      aud = "did:web:bsky.social",
                      iat = 1732526308,
                      exp = 1732598308
                    }

        bskyMockNet `loginReturns` BskySession {accessJwt, refreshJwt}

        handleEvent bskyBackend (EventNote aBskyNote)
        bskyMockNet `delaysBy` 7300
        handleEvent bskyBackend (EventNote aBskyNote)

        calledRefreshToken bskyMockNet `shouldReturn` 1

      it "discards #bsky tag when posting note" $ \(bskyMockNet, BackendHandler {handleEvent}) -> do
        handleEvent bskyBackend (EventNote aBskyNote)

        bskyMockNet `calledCreatePost` ((\p -> text p == "some note ") :: BskyPost -> Bool)

  describe "publishArticle function" $ do
    let session = BskySession defaultToken defaultToken

    it "publishes article with title metadata successfully" $ do
      result <- publishArticle successfulDoPublish bskyBackend session articleWithTitle
      case result of
        Right Record {uri, cid} -> do
          uri `shouldBe` "test-uri"
          cid `shouldBe` "test-cid"
        Left err -> fail $ "Expected Right but got Left: " <> err

    it "publishes article without metadata with empty title" $ do
      result <- publishArticle successfulDoPublish bskyBackend session articleWithoutMetadata
      case result of
        Right Record {uri, cid} -> do
          uri `shouldBe` "test-uri"
          cid `shouldBe` "test-cid"
        Left _ -> fail "Expected Right but got Left"

    it "returns Left when doPublish throws exception" $ do
      result <- publishArticle failingDoPublish bskyBackend session articleWithTitle
      case result of
        Left err -> err `shouldContain` "Failed to publish article:"
        Right _ -> fail "Expected Left but got Right"

calledCreatePost :: (HasCallStack, IsMatcher match) => BskyMockNet BskyPost -> match -> IO ()
calledCreatePost net matcher = do
  readIORef (createPostCalls net)
    >>= \calls -> case calls `matches` matcher of
      Right () -> pure ()
      Left err -> fail err

class IsMatcher m where
  matches :: [LBS.ByteString] -> m -> Either String ()

instance forall a. (FromJSON (BskyRecord a)) => IsMatcher (a -> Bool) where
  matches bposts predicate =
    if predicate (record $ fromJust $ decode @(BskyRecord a) $ head bposts)
      then Right ()
      else Left $ "Posts " <> show bposts <> " do not match predicate"

instance IsMatcher Int where
  matches bposts n =
    if length bposts == n
      then Right ()
      else Left $ "Expected " <> show n <> " posts, got " <> show (length bposts)

delaysBy :: BskyMockNet record -> Int -> IO ()
delaysBy BskyMockNet {currentTimeCalls} delay =
  modifyIORef' currentTimeCalls (const $ addUTCTime (fromIntegral delay))

loginReturns :: BskyMockNet record -> BskySession -> IO ()
loginReturns BskyMockNet {loginCalls} session =
  modifyIORef' loginCalls (const $ const session)

prop_deserialiseAuthToken :: BskyAuth -> Property
prop_deserialiseAuthToken auth =
  let token = unsafePerformIO $ serializedSampleToken auth
      deserialised = decodeAuthToken token
   in deserialised === Right auth

prop_rejectMalformedTokens :: Property
prop_rejectMalformedTokens =
  forAll arbitrary $ \(auth :: BskyAuth) ->
    let SerializedToken token = unsafePerformIO $ serializedSampleToken auth
        tampered = SerializedToken $ mconcat $ take 2 $ BS.split (fromIntegral $ ord '.') token
     in isLeft (decodeAuthToken tampered)

calledLogin :: BskyMockNet record -> IO Int
calledLogin = readIORef . loginCount

calledRefreshToken :: BskyMockNet record -> IO Int
calledRefreshToken = readIORef . refreshCount

data BskyMockNet record = BskyMockNet
  { loginCount :: IORef Int,
    createPostCalls :: IORef [LBS.ByteString],
    refreshCount :: IORef Int,
    bskyNet :: BskyNet IO,
    loginCalls :: IORef (BskyLogin -> BskySession),
    currentTimeCalls :: IORef (UTCTime -> UTCTime)
  }

newBskyMockNet :: forall record. IO (BskyMockNet record)
newBskyMockNet = do
  loginCount <- newIORef (0 :: Int)
  createPostCalls :: IORef [LBS.ByteString] <- newIORef []
  refreshCount <- newIORef (0 :: Int)
  let dummySession = BskySession defaultToken defaultToken
  loginCalls <- newIORef $ const dummySession
  currentTimeCalls <- newIORef id
  let bskyNet =
        BskyNet
          { doLogin = \_ login -> modifyIORef' loginCount succ >> readIORef loginCalls >>= \k -> pure (k login),
            doCreateRecord = \_ p -> modifyIORef' createPostCalls (mimeRender (Proxy @JSON) p :) >> pure (Record "foo" "bar"),
            doPutRecord = undefined,
            doDeleteRecord = \_ _ -> pure (),
            doRefresh = \_ -> modifyIORef' refreshCount succ >> pure dummySession,
            currentTime = \t -> readIORef currentTimeCalls >>= \k -> pure $ k t,
            doListRecords = \_ _ _ _ _ _ -> pure (ListRecordsResponse Nothing [])
          }
  pure $ BskyMockNet {..}

newtype TestDB a = TestDB {runTestDB :: ReaderT UserProfile IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader UserProfile, MonadIO, MonadThrow, MonadCatch)

newtype TestDBError = TestDBError Text
  deriving (Eq, Show)

instance Exception TestDBError

instance MonadError ServerError TestDB where
  throwError = throwM
  catchError = catch

instance DB TestDB where
  type DBError TestDB = TestDBError

  setCurrentTime _ _ = undefined
  getCurrentTime _ = undefined
  initLogStorage = pure ()
  writeEvent e =
    TestDB $
      ReaderT $
        \_ -> throwIO $ TestDBError $ pack $ "fail to write event " <> show e
  updateLatestFlow _ = undefined
  readFlow _ _ = undefined
  readEvents _ _ = undefined
  readNotes _ _ = undefined
  readGoals _ = undefined
  searchNotes _ _ = undefined
  readViews _ = undefined
  readCommands _ = undefined
  readProfile _ = ask
  readProfileById _ = ask
  writeProfile _ = pure ()
  insertProfile _ = pure ""

dbFailsToWriteEvents ::
  UserProfile ->
  FilePath ->
  FilePath ->
  LoggerEnv ->
  TestDB x ->
  IO x
dbFailsToWriteEvents user _ _ _ = (`runReaderT` user) . runTestDB

mkBackends :: IORef [Event] -> Backends
mkBackends ref = Backend.insert bskyIO Backend.empty
  where
    bskyIO :: BackendHandler BskyBackend IO
    bskyIO =
      BackendHandler
        { handleEvent = \_ event -> liftIO $ atomicModifyIORef' ref (\es -> (event : es, ()))
        }

defaultToken :: SerializedToken
{-# NOINLINE defaultToken #-}
defaultToken =
  unsafePerformIO $
    serializedSampleToken $
      BskyAuth
        { scope = "com.atproto.refresh",
          sub = "did:plc:1234567",
          aud = "did:web:bsky.social",
          iat = 1732518838,
          exp = 1732526038
        }

-- Test data for publishArticle tests
articleWithTitle :: Article
articleWithTitle =
  PublishArticle
    { _articleUser = "testuser",
      _articleTimestamp = UTCTime aDay 0,
      _articleDir = "/test/dir",
      _article =
        "---\n\
        \title: Test Article\n\
        \description: A test description\n\
        \---\n\
        \\n\
        \# Introduction\n\
        \\n\
        \This is a test article.\n"
    }

articleWithoutMetadata :: Article
articleWithoutMetadata =
  PublishArticle
    { _articleUser = "testuser",
      _articleTimestamp = UTCTime aDay 0,
      _articleDir = "/test/dir",
      _article =
        "# Introduction\n\
        \\n\
        \This is a test article without metadata.\n"
    }

-- Mock doPublish implementations
successfulDoPublish :: (MonadIO m) => a -> b -> m Record
successfulDoPublish _ _ = pure $ Record "test-uri" "test-cid"

failingDoPublish :: (MonadIO m, MonadThrow m) => a -> b -> m Record
failingDoPublish _ _ = throwM $ TestDBError "Simulated publish failure"

shouldContain :: String -> String -> IO ()
shouldContain actual expected =
  if pack expected `isInfixOf` pack actual
    then pure ()
    else fail $ "Expected string to contain '" <> expected <> "' but got: " <> actual
