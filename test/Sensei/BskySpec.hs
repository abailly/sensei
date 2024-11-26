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
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..))
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.Time (UTCTime (..))
import GHC.IO (unsafePerformIO)
import Network.URI.Extra (uriFromString)
import Preface.Log (LoggerEnv, fakeLogger)
import Sensei.API (Event (EventNote), NoteFlow (..), UserProfile (..), defaultProfile)
import Sensei.Backend (Backend (..))
import Sensei.Backend.Class (BackendHandler (..), Backends)
import qualified Sensei.Backend.Class as Backend
import Sensei.Bsky (BskyAuth (..), BskyNet (..), BskySession (..), Record (..), bskyEventHandler, decodeAuthToken)
import Sensei.Bsky.Core (BskyBackend (..), BskyLogin (..))
import Sensei.Builder (aDay, postNote, postNote_)
import Sensei.DB (DB (..))
import Sensei.Generators ()
import Sensei.Server (SerializedToken (..), makeToken)
import Sensei.TestHelper (app, putJSON_, serializedSampleToken, withApp, withBackends, withDBRunner)
import Servant.Server (ServerError)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (Spec, after_, before, describe, it, runIO, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Wai
import Test.QuickCheck (Property, (===))

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
      flow2 = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note #bsky"

  calls <- runIO (newIORef [])

  after_ (writeIORef calls []) $
    describe "POST /api/log with configured Bsky backend" $ do
      withApp (withBackends (mkBackends calls) app) $ it "propagate posted event to backend" $ do
        putJSON_ "/api/users/arnaud" profileWithBsky

        postNote_ flow2

        liftIO $ (head <$> readIORef calls) `shouldReturn` EventNote flow2

      withApp
        ( withBackends (mkBackends calls) $
            withDBRunner (dbFailsToWriteEvents profileWithBsky) app
        )
        $ it "does not propagate events to backend if DB write fails"
        $ do
          putJSON_ "/api/users/arnaud" profileWithBsky

          postNote flow2 `shouldRespondWith` 500

          liftIO $ (length <$> readIORef calls) `shouldReturn` 0

  describe "Bsky auth token" $ do
    prop "can deserialise base64-encoded auth token's claims" $ prop_deserialiseAuthToken

  before newBskyMockNet $
    describe "Bsky logic" $ do
      it "login with given credentials then post event with token" $ \bskyMockNet -> do
        BackendHandler{handleEvent} <- bskyEventHandler fakeLogger (bskyNet bskyMockNet)

        handleEvent bskyBackend (EventNote flow2)

        calledLogin bskyMockNet `shouldReturn` 1
        calledCreatePost bskyMockNet `shouldReturn` 1

      it "login only once when posting several events" $ \bskyMockNet -> do
        BackendHandler{handleEvent} <- bskyEventHandler fakeLogger (bskyNet bskyMockNet)

        handleEvent bskyBackend (EventNote flow2)
        handleEvent bskyBackend (EventNote flow2)

        calledLogin bskyMockNet `shouldReturn` 1
        calledCreatePost bskyMockNet `shouldReturn` 2

      it "discards note if it does not contain #bsky tag" $ \bskyMockNet -> do
        let notForBsky = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note #foo"
        BackendHandler{handleEvent} <- bskyEventHandler fakeLogger (bskyNet bskyMockNet)

        handleEvent bskyBackend (EventNote notForBsky)

        calledCreatePost bskyMockNet `shouldReturn` 0

prop_deserialiseAuthToken :: BskyAuth -> Property
prop_deserialiseAuthToken auth =
  let token = unsafePerformIO $ serializedSampleToken auth
      deserialised = decodeAuthToken token
   in deserialised === Right auth

-- it "refreshes token given it has expired" $ \bskyMockNet -> do
--   BackendHandler{handleEvent} <- bskyEventHandler fakeLogger (bskyNet bskyMockNet)
--   bskyMockNet `loginReturns`

--   handleEvent bskyBackend (EventNote notForBsky)
--   bskyMockNet `delaysBy`
--   handleEvent bskyBackend (EventNote notForBsky)

--   calledRefreshToken bskyMockNet `shouldReturn` 1

calledCreatePost :: BskyMockNet -> IO Int
calledCreatePost = readIORef . createPostCount

calledLogin :: BskyMockNet -> IO Int
calledLogin = readIORef . loginCount

data BskyMockNet = BskyMockNet
  { loginCount :: IORef Int
  , createPostCount :: IORef Int
  , bskyNet :: BskyNet IO
  }

newBskyMockNet :: IO BskyMockNet
newBskyMockNet = do
  loginCount <- newIORef (0 :: Int)
  createPostCount <- newIORef (0 :: Int)
  let bskyNet =
        BskyNet
          { doLogin = \_ _ -> modifyIORef' loginCount succ >> pure (BskySession "token" "refresh")
          , doCreatePost = \_ _ -> modifyIORef' createPostCount succ >> pure (Record "foo" "bar")
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
