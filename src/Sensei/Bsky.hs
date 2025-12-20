{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Sensei.Bsky
  ( module Sensei.Bsky,
    module Sensei.Bsky.Core,
    module Sensei.Bsky.Leaflet,
    module Sensei.Bsky.TID,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception.Safe (MonadCatch, SomeException, catch)
import Control.Lens ((&), (?~), (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.JWT (Audience (..), NumericDate (..), addClaim, claimAud, claimExp, claimIat, claimSub, emptyClaimsSet)
import Data.Aeson (FromJSON, ToJSON (..), Value (String), eitherDecodeStrict', object, withObject, (.:), (.=))
import Data.Aeson.Types (FromJSON (..))
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Char (ord)
import Data.Functor (void, ($>))
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text, isInfixOf, unpack)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol)
import Network.URI.Extra (uriFromString)
import Preface.Log (LoggerEnv (withLog), logInfo)
import Preface.Utils (decodeUtf8')
import Sensei.Article (Article (DeleteArticle, PublishArticle, UpdateArticle), article, articleRkey, articleTimestamp)
import Sensei.Backend.Class (BackendHandler (..))
import Sensei.Bsky.Core
import Sensei.Bsky.Leaflet
import Sensei.Bsky.Leaflet.Markdown (extractMetadata, mkMarkdownDocument)
import Sensei.Bsky.TID
import Sensei.Client.Monad (ClientConfig (..), ClientMonad, Config, send)
import Sensei.Event (Event (..))
import Sensei.Flow (noteContent, noteTimestamp)
import Sensei.Server.Auth (FromJWT, SerializedToken (..), ToJWT (encodeJWT))
import Servant
import Servant.Client.Core (clientIn)
import Prelude hiding (exp)

data BskySession = BskySession
  { accessJwt :: SerializedToken,
    refreshJwt :: SerializedToken
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type instance Lexicon BskyPost = "app.bsky.feed.post"

type instance Key BskyPost = TID

data BskyPost = BskyPost
  { text :: Text,
    createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Record = Record
  { uri :: Text,
    cid :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Response from com.atproto.repo.listRecords
data ListRecordsResponse record = ListRecordsResponse
  { cursor :: Maybe Text,
    records :: [RecordWithMetadata record]
  }
  deriving stock (Generic)

deriving instance (Show record) => Show (ListRecordsResponse record)

deriving instance (Eq record) => Eq (ListRecordsResponse record)

instance (FromJSON record) => FromJSON (ListRecordsResponse record) where
  parseJSON = withObject "ListRecordsResponse" $ \o ->
    ListRecordsResponse
      <$> o .: "cursor"
      <*> o .: "records"

instance (ToJSON record) => ToJSON (ListRecordsResponse record) where
  toJSON (ListRecordsResponse cursor recs) =
    object
      [ "cursor" .= cursor,
        "records" .= recs
      ]

newtype BearerToken = BearerToken SerializedToken
  deriving (Eq, Show)

instance ToHttpApiData BearerToken where
  toUrlPiece (BearerToken (SerializedToken bytes)) = "Bearer " <> decodeUtf8' bytes

type Login =
  "xrpc"
    :> "com.atproto.server.createSession"
    :> ReqBody '[JSON] BskyLogin
    :> Post '[JSON] BskySession

type Refresh =
  "xrpc"
    :> "com.atproto.server.refreshSession"
    :> Post '[JSON] BskySession

type CreateRecord record =
  "xrpc"
    :> "com.atproto.repo.createRecord"
    :> ReqBody '[JSON] (BskyRecord record)
    :> Post '[JSON] Record

type PutRecord record =
  "xrpc"
    :> "com.atproto.repo.putRecord"
    :> ReqBody '[JSON] (BskyRecord record)
    :> Post '[JSON] Record

type DeleteRecord record =
  "xrpc"
    :> "com.atproto.repo.deleteRecord"
    :> ReqBody '[JSON] (BskyRecord record)
    :> Post '[JSON] ()

type ListRecords record =
  "xrpc"
    :> "com.atproto.repo.listRecords"
    :> QueryParam' '[Required, Strict] "repo" BskyHandle
    :> QueryParam' '[Required, Strict] "collection" (BskyType (Lexicon record))
    :> QueryParam "limit" Int
    :> QueryParam "cursor" Text
    :> QueryParam "reverse" Bool
    :> Get '[JSON] (ListRecordsResponse record)

type BskyLoginAPI = Login :<|> Refresh

bskyLogin :: BskyLogin -> ClientMonad BskyClientConfig BskySession
bskyRefresh :: ClientMonad BskyClientConfig BskySession
bskyLogin :<|> bskyRefresh = clientIn (Proxy @BskyLoginAPI) Proxy

bskyCreateRecord ::
  forall record.
  (Sendable record) =>
  BskyRecord record ->
  ClientMonad BskyClientConfig Record
bskyCreateRecord = clientIn (Proxy @(CreateRecord record)) Proxy

bskyPutRecord ::
  forall record.
  (Sendable record) =>
  BskyRecord record ->
  ClientMonad BskyClientConfig Record
bskyPutRecord = clientIn (Proxy @(PutRecord record)) Proxy

bskyDeleteRecord ::
  forall record.
  (ToJSON (Key record), ToHttpApiData (Key record), KnownSymbol (Lexicon record), ToJSON record) =>
  BskyRecord record ->
  ClientMonad BskyClientConfig ()
bskyDeleteRecord = clientIn (Proxy @(DeleteRecord record)) Proxy

bskyListRecords ::
  forall record.
  (FromJSON record, KnownSymbol (Lexicon record)) =>
  BskyHandle ->
  BskyType (Lexicon record) ->
  Maybe Int ->
  Maybe Text ->
  Maybe Bool ->
  ClientMonad BskyClientConfig (ListRecordsResponse record)
bskyListRecords = clientIn (Proxy @(ListRecords record)) Proxy

data BskyLog
  = PostCreated {content :: !Text, session :: !BskySession}
  | UserAuthenticated {user :: !Text}
  | TokenRefreshed {refreshJwt :: !SerializedToken}
  | FailedToDecodeToken {token :: !SerializedToken}
  | ArticlePublished {title :: !Text, uri :: !Text, cid :: !Text}
  | ArticlePublishFailed {title :: !Text, error :: !Text}
  | ArticleUpdated {title :: !Text, uri :: !Text, cid :: !Text}
  | ArticleUpdateFailed {title :: !Text, error :: !Text}
  | ArticleDeleted {uri :: !Text}
  | ArticleDeleteFailed {uri :: !Text, error :: !Text}
  | MarkdownParseError {error :: !Text}
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
  { scope :: Text,
    sub :: Text,
    iat :: Int,
    exp :: Int,
    aud :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToJWT BskyAuth where
  encodeJWT BskyAuth {scope, sub, iat, exp, aud} =
    addClaim "scope" (String scope) $
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
  { doCreateRecord ::
      forall record.
      (Sendable record) =>
      BskyClientConfig ->
      BskyRecord record ->
      m Record,
    doPutRecord ::
      forall record.
      (Sendable record) =>
      BskyClientConfig ->
      BskyRecord record ->
      m Record,
    doDeleteRecord ::
      forall record.
      (ToJSON (Key record), ToHttpApiData (Key record), KnownSymbol (Lexicon record), ToJSON record) =>
      BskyClientConfig ->
      BskyRecord record ->
      m (),
    doLogin :: BskyClientConfig -> BskyLogin -> m BskySession,
    doRefresh :: BskyClientConfig -> m BskySession,
    doListRecords ::
      forall record.
      (FromJSON record, KnownSymbol (Lexicon record)) =>
      BskyClientConfig ->
      BskyHandle ->
      BskyType (Lexicon record) ->
      Maybe Int ->
      Maybe Text ->
      Maybe Bool ->
      m (ListRecordsResponse record),
    currentTime :: UTCTime -> m UTCTime
  }

defaultBskyNet :: BskyNet IO
defaultBskyNet = BskyNet {doCreateRecord, doPutRecord, doDeleteRecord, doLogin, doRefresh, doListRecords, currentTime}
  where
    doCreateRecord config = send config . bskyCreateRecord
    doPutRecord config = send config . bskyPutRecord
    doDeleteRecord config = send config . bskyDeleteRecord
    doLogin config = send config . bskyLogin
    doRefresh config = send config bskyRefresh
    doListRecords config repo collection limit cursor isReverse =
      send config $ bskyListRecords repo collection limit cursor isReverse
    currentTime = const $ liftIO getCurrentTime

type Sessions = TVar (Map.Map Text BskySession)

emptySessions :: Map.Map Text BskySession
emptySessions = mempty

-- | Authenticate with Bluesky and obtain a valid session.
--
-- This function handles three cases:
-- 1. No existing session: login with credentials
-- 2. Valid session: return it as-is
-- 3. Expired session: refresh the token
ensureAuthenticated ::
  forall m.
  (MonadIO m) =>
  LoggerEnv ->
  BskyNet m ->
  Sessions ->
  BskyBackend ->
  BskyLogin ->
  m BskySession
ensureAuthenticated logger BskyNet {doLogin, doRefresh, currentTime} sessionMap backend credentials = do
  maybeSession <- Map.lookup (identifier credentials) <$> liftIO (readTVarIO sessionMap)
  case maybeSession of
    Just session@BskySession {accessJwt, refreshJwt} ->
      case decodeAuthToken accessJwt of
        Right auth -> do
          let expires = fromIntegral $ exp auth
              issued = posixSecondsToUTCTime $ fromIntegral $ iat auth
          now <- utcTimeToPOSIXSeconds <$> currentTime issued
          if now < expires
            then pure session
            else refreshSession session refreshJwt
        Left _err -> do
          logInfo logger $ FailedToDecodeToken accessJwt
          loginNew
    Nothing -> loginNew
  where
    refreshSession session refreshJwt = do
      newSession <- withLog logger TokenRefreshed {refreshJwt} $ do
        doRefresh (BskyClientConfig backend (Just session {accessJwt = refreshJwt}))
      liftIO $
        atomically $
          modifyTVar' sessionMap $
            \sessions -> Map.insert (identifier credentials) newSession sessions
      pure newSession

    loginNew = do
      session <-
        withLog logger UserAuthenticated {user = identifier credentials} $
          doLogin (BskyClientConfig backend Nothing) credentials
      liftIO $
        atomically $
          modifyTVar' sessionMap $
            \sessions -> Map.insert (identifier credentials) session sessions
      pure session

-- | Publish an article to Bluesky PDS as a Leaflet document.
--
-- This function extracts metadata from the article, converts markdown to a LinearDocument,
-- builds a Document structure, and publishes it using the provided doPublish function.
--
-- Returns Either an error message (Left) or the created Record (Right).
publishArticle ::
  forall m.
  (MonadIO m, MonadCatch m) =>
  -- | Function to publish the record (typically doCreateRecord)
  (BskyClientConfig -> BskyRecord Document -> m Record) ->
  -- | Backend configuration
  BskyBackend ->
  -- | Authenticated session
  BskySession ->
  -- | Article to publish
  Article ->
  m (Either String Record)
publishArticle doPublish backend session articleOp = do
  let articleContent = articleOp ^. article
      (metadata, body) = extractMetadata articleContent
      lookupMeta key = lookup key metadata
      docTitle = maybe "" Prelude.id (lookupMeta "title")

  -- Convert markdown to LinearDocument
  linearDocResult <- liftIO $ mkMarkdownDocument body

  case linearDocResult of
    Left err -> pure $ Left $ "Failed to parse markdown: " <> err
    Right linearDoc -> do
      -- Generate new TID for the document
      docTid <- liftIO mkTid

      -- Use Article timestamp and format as ISO8601
      let articleTime = articleOp ^. articleTimestamp
          iso8601Time = Text.pack $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ")) articleTime

      -- Extract userDID and publicationId from backend
      let DID authorDID = userDID backend
          AtURI pubId = publicationId backend

      -- Build the Document
      let doc =
            Document
              { title = docTitle,
                description = lookupMeta "description",
                author = authorDID,
                pages = [Linear linearDoc],
                tags = Nothing,
                publishedAt = Just iso8601Time,
                postRef = Nothing,
                publication = Just pubId,
                theme = Nothing
              }

      -- Try to create and submit the record
      ( Right
          <$> doPublish
            (BskyClientConfig {backend, bskySession = Just session})
            BskyRecord
              { record = doc,
                repo = BskyHandle authorDID,
                rkey = docTid,
                collection = BskyType
              }
        )
        `catch` \(e :: SomeException) ->
          pure $ Left $ "Failed to publish article: " <> show e

updateArticle ::
  forall m.
  (MonadIO m, MonadCatch m) =>
  -- | Function to update the record (typically doPutRecord)
  (BskyClientConfig -> BskyRecord Document -> m Record) ->
  -- | Backend configuration
  BskyBackend ->
  -- | Authenticated session
  BskySession ->
  -- | TID/rkey of the article to update
  TID ->
  -- | Article content to update
  Article ->
  m (Either String Record)
updateArticle doPut backend session articleTid articleOp = do
  let articleContent = articleOp ^. article
      (metadata, body) = extractMetadata articleContent
      lookupMeta key = lookup key metadata
      docTitle = maybe "" Prelude.id (lookupMeta "title")

  -- Convert markdown to LinearDocument
  linearDocResult <- liftIO $ mkMarkdownDocument body

  case linearDocResult of
    Left err -> pure $ Left $ "Failed to parse markdown: " <> err
    Right linearDoc -> do
      -- Use Article timestamp and format as ISO8601
      let articleTime = articleOp ^. articleTimestamp
          iso8601Time = Text.pack $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ")) articleTime

      -- Extract userDID and publicationId from backend
      let DID authorDID = userDID backend
          AtURI pubId = publicationId backend

      -- Build the Document
      let doc =
            Document
              { title = docTitle,
                description = lookupMeta "description",
                author = authorDID,
                pages = [Linear linearDoc],
                tags = Nothing,
                publishedAt = Just iso8601Time,
                postRef = Nothing,
                publication = Just pubId,
                theme = Nothing
              }

      -- Try to update and submit the record
      ( Right
          <$> doPut
            (BskyClientConfig {backend, bskySession = Just session})
            BskyRecord
              { record = doc,
                repo = BskyHandle authorDID,
                rkey = articleTid, -- Use provided TID instead of generating new one
                collection = BskyType
              }
        )
        `catch` \(e :: SomeException) ->
          pure $ Left $ "Failed to update article: " <> show e

-- | An Event handler that transforms `FlowNote` into `BskyPost`s.
--
-- This requires a function to hander `ClientMonad`'s requests.
bskyEventHandler ::
  forall m.
  (MonadIO m, MonadCatch m) =>
  LoggerEnv ->
  BskyNet m ->
  m (BackendHandler BskyBackend m)
bskyEventHandler logger bskyNet@BskyNet {doCreateRecord, doPutRecord, doDeleteRecord} = do
  sessionMap <- liftIO $ newTVarIO emptySessions
  pure $ BackendHandler {handleEvent = handleEvent sessionMap}
  where
    postWith backend session repo note = do
      postTid <- liftIO mkTid
      withLog logger PostCreated {content = note ^. noteContent, session} $
        void $
          doCreateRecord (BskyClientConfig {backend, bskySession = Just session}) $
            BskyRecord
              { record =
                  BskyPost
                    { text = removeTag (note ^. noteContent),
                      createdAt = note ^. noteTimestamp
                    },
                -- TODO: test me!
                repo,
                rkey = postTid,
                collection = BskyType
              }

    handleEvent :: Sessions -> BskyBackend -> Event -> m ()
    handleEvent sessionMap backend = \case
      EventNote note | "#bsky" `isInfixOf` (note ^. noteContent) -> do
        let credentials = login backend
            repo = BskyHandle $ identifier credentials
        session <- ensureAuthenticated logger bskyNet sessionMap backend credentials
        postWith backend session repo note
      EventArticle articleOp@(PublishArticle {}) -> do
        let credentials = login backend
            (metadata, _) = extractMetadata (articleOp ^. article)
            docTitle = maybe "" Prelude.id (lookup "title" metadata)
        session <- ensureAuthenticated logger bskyNet sessionMap backend credentials
        result <- publishArticle doCreateRecord backend session articleOp
        case result of
          Left err ->
            logInfo logger $
              ArticlePublishFailed
                { title = docTitle,
                  error = Text.pack err
                }
          Right Record {uri = resultUri, cid = resultCid} ->
            logInfo logger $
              ArticlePublished
                { title = docTitle,
                  uri = resultUri,
                  cid = resultCid
                }
      EventArticle articleOp@(UpdateArticle {}) -> do
        let credentials = login backend
            (metadata, _) = extractMetadata (articleOp ^. article)
            docTitle = fromMaybe "" (lookup "title" metadata)
            rkey = articleOp ^. articleRkey
        session <- ensureAuthenticated logger bskyNet sessionMap backend credentials
        -- Parse TID from text
        let tidResult = maybe (Left $ "Invalid TID: " <> rkey) Right (tidFromText rkey)
        case tidResult of
          Left err ->
            logInfo logger $
              ArticleUpdateFailed
                { title = docTitle,
                  error = err
                }
          Right articleTid -> do
            result <- updateArticle doPutRecord backend session articleTid articleOp
            case result of
              Left err ->
                logInfo logger $
                  ArticleUpdateFailed
                    { title = docTitle,
                      error = Text.pack err
                    }
              Right Record {uri = resultUri, cid = resultCid} ->
                logInfo logger $
                  ArticleUpdated
                    { title = docTitle,
                      uri = resultUri,
                      cid = resultCid
                    }
      EventArticle articleOp@(DeleteArticle {}) -> do
        let credentials = login backend
            DID authorDID = userDID backend
            rkey = articleOp ^. articleRkey
        session <- ensureAuthenticated logger bskyNet sessionMap backend credentials
        -- Parse TID from text
        let tidResult = maybe (Left $ "Invalid TID: " <> rkey) Right (tidFromText rkey)
        case tidResult of
          Left err ->
            logInfo logger $
              ArticleDeleteFailed
                { uri = rkey,
                  error = err
                }
          Right articleTid -> do
            let record :: BskyRecord Document =
                  BskyRecord
                    { repo = BskyHandle authorDID,
                      collection = BskyType,
                      rkey = articleTid,
                      record = undefined -- FIXME
                    }
                config = BskyClientConfig {backend, bskySession = Just session}
            result <-
              (doDeleteRecord config record $> Right ())
                `catch` \(e :: SomeException) ->
                  pure $ Left $ show e
            case result of
              Left err ->
                logInfo logger $
                  ArticleDeleteFailed
                    { uri = rkey,
                      error = Text.pack err
                    }
              Right () ->
                logInfo logger $
                  ArticleDeleted
                    { uri = rkey
                    }
      _ -> pure ()

removeTag :: Text -> Text
removeTag content =
  let (tag, rest) = Text.breakOn "#bsky" content
   in tag <> Text.drop 5 rest

data BskyClientConfig = BskyClientConfig
  { backend :: BskyBackend,
    bskySession :: Maybe BskySession
  }
  deriving (Eq, Show)

type instance Config BskyBackend = BskyClientConfig

instance ClientConfig BskyClientConfig where
  defConfig =
    BskyClientConfig
      { backend =
          BskyBackend
            { login = BskyLogin "" "",
              pdsUrl = fromJust $ uriFromString "http://localhost:12345",
              userDID = "",
              publicationId = ""
            },
        bskySession = Nothing
      }

  additionalHeaders _ hdrs = hdrs

  setServerUri pdsUrl config@BskyClientConfig {backend} = config {backend = backend {pdsUrl}}

  getServerUri BskyClientConfig {backend = BskyBackend {pdsUrl}} = pdsUrl

  setAuthToken token config@BskyClientConfig {bskySession} = config {bskySession = updateLogin}
    where
      updateLogin = do
        l <- bskySession
        t <- token
        pure $ l {accessJwt = t}
  getAuthToken BskyClientConfig {bskySession} = accessJwt <$> bskySession
