{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Sensei.Bsky where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Lens ((&), (?~), (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.JWT (Audience (..), NumericDate (..), addClaim, claimAud, claimExp, claimIat, claimSub, emptyClaimsSet)
import Data.Aeson (FromJSON, ToJSON (..), eitherDecodeStrict', withText)
import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON (..))
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Char (ord)
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Data.Text (Text, isInfixOf, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.URI.Extra (uriFromString)
import Preface.Log (LoggerEnv (withLog), logInfo)
import Sensei.Backend.Class (BackendHandler (..))
import Sensei.Bsky.Core
import Sensei.Client.Monad (ClientConfig (..), ClientMonad, Config, send)
import Sensei.Event (Event (..))
import Sensei.Flow (noteContent, noteTimestamp)
import Sensei.Server.Auth (FromJWT, SerializedToken (..), ToJWT (encodeJWT))
import Servant
import Servant.Client.Core (clientIn)
import Prelude hiding (exp)

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

type Refresh =
  "xrpc"
    :> "com.atproto.server.refreshSession"
    :> Post '[JSON] BskySession

type CreatePost =
  "xrpc"
    :> "com.atproto.repo.createRecord"
    :> ReqBody '[JSON] BskyPost
    :> Post '[JSON] Record

type BskyAPI = Login :<|> Refresh :<|> CreatePost

bskyCreatePost :: BskyPost -> ClientMonad BskyClientConfig Record
bskyLogin :: BskyLogin -> ClientMonad BskyClientConfig BskySession
bskyRefresh :: ClientMonad BskyClientConfig BskySession
bskyLogin :<|> bskyRefresh :<|> bskyCreatePost = clientIn (Proxy @BskyAPI) Proxy

data BskyLog
  = PostCreated {content :: !Text, session :: !BskySession}
  | UserAuthenticated {user :: !Text}
  | TokenRefreshed {refreshJwt :: !SerializedToken}
  | FailedToDecodeToken {token :: !SerializedToken}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Sample refresh token
-- {
--   "scope": "com.atproto.refresh",
--   "sub": "did:plc:s5wwr2ccjuqricdxiyiuspfv",
--   "aud": "did:web:bsky.social",
--   "jti": "jEAdP+SRhbZ2WGShPl9lEfxk+y+3SCSPUvkbSpJPGlY",
--   "iat": 1732518838,
--   "exp": 1740294838
-- }

data BskyAuth = BskyAuth
  { scope :: Text
  , sub :: Text
  , iat :: Int
  , exp :: Int
  , aud :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToJWT BskyAuth where
  encodeJWT BskyAuth{scope, sub, iat, exp, aud} =
    addClaim "scope" (A.String scope) $
      emptyClaimsSet
        & claimAud ?~ Audience [fromString $ unpack aud]
        -- FIXME: there's a stringOrUri prism in JOSE package but I have no idea how to use it :(
        & claimIat ?~ NumericDate (posixSecondsToUTCTime $ fromIntegral iat)
        & claimExp ?~ NumericDate (posixSecondsToUTCTime $ fromIntegral exp)
        & claimSub ?~ fromString (unpack sub)

instance FromJWT BskyAuth

decodeAuthToken :: SerializedToken -> Either String BskyAuth
decodeAuthToken (SerializedToken bytes) =
  case BS.split (fromIntegral $ ord '.') bytes of
    [_, tok, _] -> first show $ eitherDecodeStrict' decoded
     where
      decoded = Base64.decodeLenient tok
    _other -> Left $ "invalid token format: " <> show bytes

-- | Low-level handle to send requests to PDS with some configuration.
data BskyNet m = BskyNet
  { doCreatePost :: BskyClientConfig -> BskyPost -> m Record
  , doLogin :: BskyClientConfig -> BskyLogin -> m BskySession
  , doRefresh :: BskyClientConfig -> m BskySession
  , currentTime :: UTCTime -> m UTCTime
  }

defaultBskyNet :: BskyNet IO
defaultBskyNet = BskyNet{doCreatePost, doLogin, doRefresh, currentTime}
 where
  doCreatePost config = send config . bskyCreatePost
  doLogin config = send config . bskyLogin
  doRefresh config = send config bskyRefresh
  currentTime = const $ liftIO getCurrentTime

-- | An Event handler that transforms `FlowNote` into `BskyPost`s.
--
-- This requires a function to hander `ClientMonad`'s requests.
bskyEventHandler ::
  forall m.
  MonadIO m =>
  LoggerEnv ->
  BskyNet m ->
  m (BackendHandler BskyBackend m)
bskyEventHandler logger BskyNet{doCreatePost, doLogin, doRefresh, currentTime} = do
  sessionMap <- liftIO $ newTVarIO mempty
  pure $ BackendHandler{handleEvent = handleEvent sessionMap}
 where
  postWith backend session repo note =
    withLog logger PostCreated{content = note ^. noteContent, session} $
      void $
        doCreatePost (BskyClientConfig{backend, bskySession = Just session}) $
          BskyPost
            { record =
                BskyContent
                  { text = note ^. noteContent
                  , createdAt = note ^. noteTimestamp
                  }
            , -- TODO: test me!
              repo
            , collection = BskyType
            }

  handleEvent :: TVar (Map.Map Text BskySession) -> BskyBackend -> Event -> m ()
  handleEvent sessionMap backend = \case
    EventNote note | "#bsky" `isInfixOf` (note ^. noteContent) -> do
      let credentials = login backend
          repo = BskyHandle $ identifier credentials
      maybeSession <- Map.lookup (identifier credentials) <$> liftIO (readTVarIO sessionMap)
      case maybeSession of
        Just session@BskySession{accessJwt, refreshJwt} ->
          case decodeAuthToken accessJwt of
            Right auth -> do
              let expires = fromIntegral $ exp auth
                  issued = posixSecondsToUTCTime $ fromIntegral $ iat auth
              now <- utcTimeToPOSIXSeconds <$> currentTime issued
              if now < expires
                then postWith backend session repo note
                else do
                  newSession <- withLog logger TokenRefreshed{refreshJwt} $ do
                    doRefresh (BskyClientConfig backend (Just session{accessJwt = refreshJwt}))
                  liftIO $
                    atomically $
                      modifyTVar' sessionMap $
                        \sessions -> Map.insert (identifier credentials) newSession sessions
                  postWith backend newSession repo note
            Left _err ->
              logInfo logger $ FailedToDecodeToken accessJwt
        Nothing -> do
          session <-
            withLog logger UserAuthenticated{user = identifier credentials} $
              doLogin (BskyClientConfig backend Nothing) credentials
          liftIO $
            atomically $
              modifyTVar' sessionMap $
                \sessions -> Map.insert (identifier credentials) session sessions
          postWith backend session repo note
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
