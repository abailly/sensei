{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn #-}

module Sensei.BskySpec where

import Control.Monad.IO.Class (liftIO)
import Data.Data (Proxy (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust)
import Data.Time (UTCTime (..))
import Network.URI.Extra (uriFromString)
import Sensei.API (NoteFlow (..), UserProfile (..), defaultProfile)
import Sensei.App (AppM)
import Sensei.Backend (Backend (..))
import Sensei.Backend.Class (BackendIO (..), Backends)
import qualified Sensei.Backend.Class as Backend
import Sensei.Bsky (BskyClientConfig, backend)
import Sensei.Bsky.Core (BskyBackend (..), BskyLogin (..))
import Sensei.Builder (aDay, postNote_)
import Sensei.Generators ()
import Sensei.TestHelper (app, putJSON_, withApp, withBackends)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (Spec, it, runIO, shouldReturn)

spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @BskyBackend)

  calls <- runIO (newIORef [])

  withApp (withBackends (mkBackends calls) app) $
    it "POST /api/log with configured Bsky backend config leads to backend being called" $ do
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
          flow2 = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note"

      putJSON_ "/api/users/arnaud" profileWithBsky

      postNote_ flow2

      liftIO $ (backend . head <$> readIORef calls) `shouldReturn` bskyBackend

mkBackends :: IORef [BskyClientConfig] -> Backends
mkBackends ref = Backend.insert bskyIO Backend.empty
 where
  bskyIO :: BackendIO BskyBackend AppM
  bskyIO =
    BackendIO
      { send = \config _a -> liftIO $ atomicModifyIORef' ref (\cs -> (config : cs, undefined))
      }
