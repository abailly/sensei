{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Sensei.BskySpec (spec) where

import Control.Exception (Exception, throwIO)
import Control.Exception.Safe (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT), ask, runReaderT)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Data (Proxy (..))
import Data.Either (isLeft)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.Time (UTCTime (..), addUTCTime)
import GHC.IO (unsafePerformIO)
import Network.URI.Extra (uriFromString)
import Preface.Log (LoggerEnv, fakeLogger)
import Sensei.API (Event (EventNote), NoteFlow (..), UserProfile (..), defaultProfile)
import Sensei.Backend (Backend (..))
import Sensei.Backend.Class (BackendHandler (..), Backends)
import qualified Sensei.Backend.Class as Backend
import Sensei.Bsky (BskyAuth (..), BskyNet (..), BskyPost (..), BskySession (..), Record (..), bskyEventHandler, decodeAuthToken, text)
import Sensei.Bsky.Core (BskyBackend (..), BskyLogin (..))
import Sensei.Builder (aDay, postNote, postNote_)
import Sensei.DB (DB (..))
import Sensei.Generators ()
import Sensei.Server (SerializedToken (..))
import Sensei.TestHelper (app, putJSON_, serializedSampleToken, withApp, withBackends, withDBRunner)
import Servant.Server (ServerError)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (Expectation, Spec, after_, before, describe, it, runIO, shouldReturn)
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
                { identifier = "bob.bsky.social"
                , password = "password"
                }
          , pdsUrl = fromJust $ uriFromString "https://some.social"
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

  before newBskyMockNet $
    describe "Bsky logic" $ do
      it "login with given credentials then post event with token" $ \bskyMockNet -> do
        BackendHandler{handleEvent} <- bskyEventHandler fakeLogger (bskyNet bskyMockNet)

        handleEvent bskyBackend (EventNote aBskyNote)

        calledLogin bskyMockNet `shouldReturn` 1
        calledCreatePost bskyMockNet `shouldReturn` 1

      it "login only once when posting several events" $ \bskyMockNet -> do
        BackendHandler{handleEvent} <- bskyEventHandler fakeLogger (bskyNet bskyMockNet)

        handleEvent bskyBackend (EventNote aBskyNote)
        handleEvent bskyBackend (EventNote aBskyNote)

        calledLogin bskyMockNet `shouldReturn` 1
        calledCreatePost bskyMockNet `shouldReturn` 2

      it "discards note if it does not contain #bsky tag" $ \bskyMockNet -> do
        let notForBsky = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note #foo"
        BackendHandler{handleEvent} <- bskyEventHandler fakeLogger (bskyNet bskyMockNet)

        handleEvent bskyBackend (EventNote notForBsky)

        calledCreatePost bskyMockNet `shouldReturn` 0

      it "refreshes token given it has expired" $ \bskyMockNet -> do
        BackendHandler{handleEvent} <- bskyEventHandler fakeLogger (bskyNet bskyMockNet)
        let accessJwt = defaultToken
            refreshJwt =
              unsafePerformIO $
                serializedSampleToken $
                  BskyAuth
                    { scope = "com.atproto.refresh"
                    , sub = "did:plc:234567"
                    , aud = "did:web:bsky.social"
                    , iat = 1732526308
                    , exp = 1732598308
                    }

        bskyMockNet `loginReturns` BskySession{accessJwt, refreshJwt}

        handleEvent bskyBackend (EventNote aBskyNote)
        bskyMockNet `delaysBy` 7300
        handleEvent bskyBackend (EventNote aBskyNote)

        calledRefreshToken bskyMockNet `shouldReturn` 1

      it "discards #bsky tag when posting note" $ \bskyMockNet -> do
        BackendHandler{handleEvent} <- bskyEventHandler fakeLogger (bskyNet bskyMockNet)

        handleEvent bskyBackend (EventNote aBskyNote)

        bskyMockNet `calledCreatePost'` WithArgumentMatching ((== "some note ") . text . content)

calledCreatePost' :: BskyMockNet -> Matcher -> Expectation
calledCreatePost' net matcher = do
  readIORef (createPostCalls net)
    >>= \calls -> case head calls `matches` matcher of
      Right () -> pure ()
      Left err -> fail err

matches :: BskyPost -> Matcher -> Either String ()
matches bpost (WithArgumentMatching predicate) =
  if predicate bpost
    then Right ()
    else Left $ "Post " <> show bpost <> " does not match"

data Matcher = WithArgumentMatching (BskyPost -> Bool)

delaysBy :: BskyMockNet -> Int -> IO ()
delaysBy BskyMockNet{currentTimeCalls} delay =
  modifyIORef' currentTimeCalls (const $ addUTCTime (fromIntegral delay))

loginReturns :: BskyMockNet -> BskySession -> IO ()
loginReturns BskyMockNet{loginCalls} session =
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

calledCreatePost :: BskyMockNet -> IO Int
calledCreatePost = fmap length . readIORef . createPostCalls

calledLogin :: BskyMockNet -> IO Int
calledLogin = readIORef . loginCount

calledRefreshToken :: BskyMockNet -> IO Int
calledRefreshToken = readIORef . refreshCount

data BskyMockNet = BskyMockNet
  { loginCount :: IORef Int
  , createPostCalls :: IORef [BskyPost]
  , refreshCount :: IORef Int
  , bskyNet :: BskyNet IO
  , loginCalls :: IORef (BskyLogin -> BskySession)
  , currentTimeCalls :: IORef (UTCTime -> UTCTime)
  }

newBskyMockNet :: IO BskyMockNet
newBskyMockNet = do
  loginCount <- newIORef (0 :: Int)
  createPostCalls <- newIORef []
  refreshCount <- newIORef (0 :: Int)
  let dummySession = BskySession defaultToken defaultToken
  loginCalls <- newIORef $ const dummySession
  currentTimeCalls <- newIORef id
  let bskyNet =
        BskyNet
          { doLogin = \_ login -> modifyIORef' loginCount succ >> readIORef loginCalls >>= \k -> pure (k login)
          , doCreatePost = \_ post -> modifyIORef' createPostCalls (post :) >> pure (Record "foo" "bar")
          , doRefresh = \_ -> modifyIORef' refreshCount succ >> pure dummySession
          , currentTime = \t -> readIORef currentTimeCalls >>= \k -> pure $ k t
          }
  pure $ BskyMockNet{..}

newtype TestDB a = TestDB {runTestDB :: ReaderT UserProfile IO a}
  deriving (Functor, Applicative, Monad, MonadReader UserProfile, MonadIO, MonadThrow, MonadCatch)

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
        { scope = "com.atproto.refresh"
        , sub = "did:plc:1234567"
        , aud = "did:web:bsky.social"
        , iat = 1732518838
        , exp = 1732526038
        }
