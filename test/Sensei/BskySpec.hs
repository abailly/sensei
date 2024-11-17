{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.BskySpec where

import Control.Monad.IO.Class (liftIO)
import Data.Data (Proxy (..))
import Data.Maybe (fromJust)
import Data.Time (UTCTime (..))
import Network.URI.Extra (uriFromString)
import Sensei.API (NoteFlow (..), UserProfile (..), defaultProfile)
import Sensei.Backend (Backend (..))
import Sensei.Bsky.Core (BskyBackend (..), BskyLogin (..))
import Sensei.Builder (aDay, postNote_)
import Sensei.Generators ()
import Sensei.TestHelper (app, postJSON_, withApp)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (Spec, it, pendingWith)

spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @BskyBackend)

  withApp app $
    it "POST /api/log with configured Bksy account authenticates user and send post" $ do
      let profileWithBsky =
            defaultProfile
              { backends =
                  [ Backend $
                      BskyBackend
                        { login =
                            BskyLogin
                              { identifier = "bob.bsky.social"
                              , password = "password"
                              }
                        , pdsUrl = fromJust $ uriFromString "https://some.social"
                        }
                  ]
              }
          flow2 = NoteFlow "arnaud" (UTCTime aDay 0) "some/directory" "some note"

      postJSON_ "/api/users" profileWithBsky

      postNote_ flow2

      liftIO $ pendingWith "check bsky has been called"
