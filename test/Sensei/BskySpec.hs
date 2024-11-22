{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.BskySpec where

import Control.Monad.IO.Class (liftIO)
import Data.Data (Proxy (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust)
import Data.Time (UTCTime (..))
import Network.URI.Extra (uriFromString)
import Sensei.API (Event (EventNote), NoteFlow (..), UserProfile (..), defaultProfile)
import Sensei.App (AppM)
import Sensei.Backend (Backend (..))
import Sensei.Backend.Class (BackendHandler (..), Backends)
import qualified Sensei.Backend.Class as Backend
import Sensei.Bsky.Core (BskyBackend (..), BskyLogin (..))
import Sensei.Builder (aDay, postNote_)
import Sensei.DB.SQLite (SQLiteDB)
import Sensei.Generators ()
import Sensei.TestHelper (app, putJSON_, withApp, withBackends)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (Spec, it, runIO, shouldReturn)

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

  withApp (withBackends (mkBackends calls) app) $ do
    it "POST /api/log with configured Bsky backend config leads to backend being called" $ do
      let flow2 = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note"

      putJSON_ "/api/users/arnaud" profileWithBsky

      postNote_ flow2

      liftIO $ (head <$> readIORef calls) `shouldReturn` EventNote flow2

    it "POST /api/log does not propagate events to backend if write fails" $ do
      let flow2 = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note"

      putJSON_ "/api/users/arnaud" profileWithBsky

      postNote_ flow2

      liftIO $ (head <$> readIORef calls) `shouldReturn` EventNote flow2

mkBackends :: IORef [Event] -> Backends
mkBackends ref = Backend.insert bskyIO Backend.empty
 where
  bskyIO :: BackendHandler BskyBackend (AppM SQLiteDB)
  bskyIO =
    BackendHandler
      { handleEvent = \_ event -> liftIO $ atomicModifyIORef' ref (\es -> (event : es, ()))
      }
