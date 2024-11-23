{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Sensei.BskySpec where

import Control.Exception (Exception, throwIO)
import Control.Exception.Safe (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT), ask, runReaderT)
import Data.Data (Proxy (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import Data.Time (UTCTime (..))
import Network.URI.Extra (uriFromString)
import Preface.Codec (Encoded, Hex)
import Preface.Log (LoggerEnv)
import Sensei.API (Event (EventNote), NoteFlow (..), UserProfile (..), defaultProfile)
import Sensei.App (AppM)
import Sensei.Backend (Backend (..))
import Sensei.Backend.Class (BackendHandler (..), Backends)
import qualified Sensei.Backend.Class as Backend
import Sensei.Bsky (BskyAPI, BskyPost, BskySession, Record, bskyEventHandler)
import Sensei.Bsky.Core (BskyBackend (..), BskyLogin (..))
import Sensei.Builder (aDay, postNote, postNote_)
import Sensei.DB (DB (..))
import Sensei.DB.SQLite (SQLiteDB)
import Sensei.Generators ()
import Sensei.TestHelper (app, putJSON_, shouldRespondWith, withApp, withBackends, withDBRunner)
import Sensei.WaiTestHelper (runRequestWith)
import Servant.API (type (:<|>) ((:<|>)))
import Servant.Server (Application, Handler, ServerError, serve)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (Spec, SpecWith, after_, around, describe, it, runIO, shouldReturn)

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

  calls <- runIO (newIORef [])

  after_ (writeIORef calls []) $
    describe "POST /api/log with configured Bsky backend" $ do
      withApp (withBackends (mkBackends (Proxy @SQLiteDB) calls) app) $ do
        it "propagate posted event to backend" $ do
          let flow2 = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note"

          putJSON_ "/api/users/arnaud" profileWithBsky

          postNote_ flow2

          liftIO $ (head <$> readIORef calls) `shouldReturn` EventNote flow2

      withApp
        ( withBackends (mkBackends (Proxy @TestDB) calls) $
            withDBRunner (dbFailsToWriteEvents profileWithBsky) $
              app
        )
        $ do
          it "does not propagate events to backend if DB write fails" $ do
            let flow2 = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note"

            putJSON_ "/api/users/arnaud" profileWithBsky

            postNote flow2 `shouldRespondWith` 500

            liftIO $ (length <$> readIORef calls) `shouldReturn` 0

  withMock bskyMock $
    describe "Bsky Backend" $ do
      it "login with given credentials then post event with token" $ do
        let handler = bskyEventHandler runRequestWith
            flow2 = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note"

        handleEvent handler bskyBackend (EventNote flow2)

withMock :: IO Application -> SpecWith (Maybe (Encoded Hex), Application) -> Spec
withMock makeApp = around mkApp
 where
  mkApp act = do
    server <- makeApp
    act (Nothing, server)

bskyMock :: IO Application
bskyMock = pure $ serve (Proxy @BskyAPI) (mockLogin :<|> mockPost)
 where
  mockPost :: BskyPost -> Handler Record
  mockPost = undefined

  mockLogin :: BskyLogin -> Handler BskySession
  mockLogin = undefined

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

mkBackends :: forall db. (MonadIO db, DB db) => Proxy db -> IORef [Event] -> Backends
mkBackends _ ref = Backend.insert bskyIO Backend.empty
 where
  bskyIO :: BackendHandler BskyBackend (AppM db)
  bskyIO =
    BackendHandler
      { handleEvent = \_ event -> liftIO $ atomicModifyIORef' ref (\es -> (event : es, ()))
      }
