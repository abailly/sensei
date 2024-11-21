{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.Bsky where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON (..), withText)
import Data.Aeson.Types (FromJSON (..))
import Data.Functor (void)
import Data.Maybe (fromJust)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.URI.Extra (uriFromString)
import Sensei.Backend.Class (BackendHandler (..))
import Sensei.Bsky.Core
import Sensei.Client.Monad (ClientConfig (..), ClientMonad, Config)
import Sensei.Event (Event (..))
import Sensei.Flow (noteContent, noteTimestamp)
import Sensei.Server.Auth (SerializedToken (..))
import Servant
import Servant.Client.Core (clientIn)

data BskySession = BskySession
  { accessJwt :: SerializedToken
  , refreshJwt :: SerializedToken
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype BskyHandle = BskyHandle Text
  deriving newtype (Eq, Show, IsString, ToJSON, FromJSON)

data BskyType (bskyType :: Symbol) = BskyType

instance KnownSymbol bskyType => Show (BskyType bskyType) where
  show _ = symbolVal (Proxy :: Proxy bskyType)

instance Eq (BskyType bskyType) where
  _ == _ = True

instance KnownSymbol bskyType => ToJSON (BskyType bskyType) where
  toJSON _ = toJSON $ symbolVal (Proxy :: Proxy bskyType)

instance KnownSymbol bskyType => FromJSON (BskyType bskyType) where
  parseJSON = withText "Bsky type" $ \txt ->
    if txt == Text.pack (symbolVal (Proxy :: Proxy bskyType))
      then pure BskyType
      else fail ("unexpected Bsky type " <> Text.unpack txt)

data BskyPost = BskyPost
  { repo :: BskyHandle
  , collection :: BskyType "app.bsky.feed.post"
  , record :: BskyContent
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data BskyContent = BskyContent
  { text :: Text
  , createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Record = Record
  { uri :: Text
  , cid :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype BearerToken = BearerToken SerializedToken
  deriving (Eq, Show)

instance ToHttpApiData BearerToken where
  toUrlPiece (BearerToken (SerializedToken bytes)) = "Bearer " <> decodeUtf8 bytes

type Login =
  "xrpc"
    :> "com.atproto.server.createSession"
    :> ReqBody '[JSON] BskyLogin
    :> Post '[JSON] BskySession

type CreatePost =
  "xrpc"
    :> "com.atproto.repo.createRecord"
    :> ReqBody '[JSON] BskyPost
    :> Post '[JSON] Record

type BskyAPI = Login :<|> CreatePost

bskyCreatePost :: BskyPost -> ClientMonad BskyClientConfig Record
bskyLogin :: BskyLogin -> ClientMonad BskyClientConfig BskySession
bskyLogin :<|> bskyCreatePost = clientIn (Proxy @BskyAPI) Proxy

-- | An Event handler that transforms `FlowNote` into `BskyPost`s.
--
-- This requires a function to hander `ClientMonad`'s requests.
bskyEventHandler ::
  forall m.
  Monad m =>
  (forall a. BskyClientConfig -> ClientMonad BskyClientConfig a -> m a) ->
  BackendHandler BskyBackend m
bskyEventHandler send = BackendHandler{handleEvent}
 where
  handleEvent :: Monad m => BskyBackend -> Event -> m ()
  handleEvent backend = \case
    EventNote note -> do
      let config = BskyClientConfig backend Nothing
      session <- send config $ bskyLogin (login backend)
      void $
        send (config{bskySession = Just session}) $
          bskyCreatePost
            BskyPost
              { record =
                  BskyContent
                    { text = note ^. noteContent
                    , createdAt = note ^. noteTimestamp
                    }
              , repo = ""
              , collection = BskyType
              }
    _ -> pure ()

data BskyClientConfig = BskyClientConfig
  { backend :: BskyBackend
  , bskySession :: Maybe BskySession
  }
  deriving (Eq, Show)

type instance Config BskyBackend = BskyClientConfig

instance ClientConfig BskyClientConfig where
  defConfig =
    BskyClientConfig
      { backend =
          BskyBackend
            { login = BskyLogin "" ""
            , pdsUrl = fromJust $ uriFromString "http://localhost:12345"
            }
      , bskySession = Nothing
      }

  additionalHeaders _ hdrs = hdrs

  setServerUri pdsUrl config@BskyClientConfig{backend} = config{backend = backend{pdsUrl}}

  getServerUri BskyClientConfig{backend = BskyBackend{pdsUrl}} = pdsUrl

  setAuthToken token config@BskyClientConfig{bskySession} = config{bskySession = updateLogin}
   where
    updateLogin = do
      l <- bskySession
      t <- token
      pure $ l{accessJwt = t}
  getAuthToken BskyClientConfig{bskySession} = accessJwt <$> bskySession
