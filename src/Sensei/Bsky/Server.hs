{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test server implementation for Bluesky API subset.
--
-- This module provides a WAI Application wrapping a Servant server that implements
-- the subset of the Bluesky API that Sensei interacts with, primarily for testing purposes.
module Sensei.Bsky.Server
  ( -- * Server Application
    bskyTestApp,
    bskyTestAppWithState,

    -- * Server State
    BskyServerState (..),
    newBskyServerState,

    -- * API Implementation
    BskyTestAPI,
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.Wai (Application)
import Sensei.Bsky
  ( BskyAuth (..),
    BskyPost,
    BskyRecord (..),
    BskySession (..),
    CreateRecord,
    ListRecords,
    ListRecordsResponse (..),
    Login,
    PutRecord,
    Record (..),
    Refresh,
  )
import Sensei.Bsky.Core (BskyHandle (..), BskyLogin (..), BskyType (..), Lexicon)
import Sensei.Server.Auth (JWTSettings, SerializedToken (..), makeToken)
import Servant
  ( Handler,
    Proxy (..),
    serve,
    type (:<|>) (..),
  )

-- * API Definition

-- | Subset of Bluesky API that Sensei uses
type BskyTestAPI =
  ( Login
      :<|> Refresh
      :<|> CreateRecord BskyPost
      :<|> PutRecord BskyPost
      :<|> ListRecords BskyPost
  )

-- * Server State

-- | State maintained by the test Bluesky server
data BskyServerState = BskyServerState
  { -- | Sessions keyed by user identifier
    sessions :: TVar (Map Text BskySession),
    -- | Posts created via createRecord
    createdPosts :: IORef [BskyRecord BskyPost],
    -- | Posts updated via putRecord
    updatedPosts :: IORef [BskyRecord BskyPost],
    -- | Number of login calls
    loginCalls :: IORef Int,
    -- | Number of refresh calls
    refreshCalls :: IORef Int,
    -- | Valid credentials for authentication
    validCredentials :: Map Text Text,
    -- | DID for each user (identifier -> DID)
    userDIDs :: Map Text Text,
    -- | Audience for JWT tokens
    jwtAudience :: Text,
    -- | JWT settings for signing tokens
    jwtSettings :: JWTSettings
  }

-- | Create new server state with credentials and DIDs
newBskyServerState :: Map Text Text -> Map Text Text -> Text -> JWTSettings -> IO BskyServerState
newBskyServerState validCredentials userDIDs jwtAudience jwtSettings = do
  sessions <- newTVarIO Map.empty
  createdPosts <- newIORef []
  updatedPosts <- newIORef []
  loginCalls <- newIORef 0
  refreshCalls <- newIORef 0
  pure BskyServerState {..}

-- * Server Implementation

-- | Create a JWT token for a given user
createJWT :: JWTSettings -> Text -> Text -> Text -> IO SerializedToken
createJWT jwtSettings did audience scope = do
  now <- getCurrentTime
  let iat = floor $ utcTimeToPOSIXSeconds now
      exp' = iat + (90 * 24 * 3600) -- 90 days expiry
      auth = BskyAuth scope did iat exp' audience
  makeToken jwtSettings auth

-- | Create a WAI Application for the Bsky test server with default state
bskyTestApp :: Map Text Text -> Map Text Text -> Text -> JWTSettings -> IO Application
bskyTestApp validCredentials userDIDs jwtAudience jwtSettings = do
  state <- newBskyServerState validCredentials userDIDs jwtAudience jwtSettings
  pure $ bskyTestAppWithState state

-- | Create a WAI Application for the Bsky test server with provided state
bskyTestAppWithState :: BskyServerState -> Application
bskyTestAppWithState state =
  serve (Proxy @BskyTestAPI) $
    loginHandler state
      :<|> refreshHandler state
      :<|> createRecordHandler state
      :<|> putRecordHandler state
      :<|> listRecordsHandler state

-- | Handle login requests
loginHandler :: BskyServerState -> BskyLogin -> Handler BskySession
loginHandler BskyServerState {sessions, loginCalls, validCredentials, userDIDs, jwtAudience, jwtSettings} login = liftIO $ do
  -- Increment login counter
  atomicModifyIORef' loginCalls (\n -> (n + 1, ()))

  -- Validate credentials
  case (Map.lookup (identifier login) validCredentials, Map.lookup (identifier login) userDIDs) of
    (Just validPassword, Just did) | validPassword == password login -> do
      -- Generate access and refresh tokens
      accessJwt <- createJWT jwtSettings did jwtAudience "com.atproto.access"
      refreshJwt <- createJWT jwtSettings did jwtAudience "com.atproto.refresh"

      let session = BskySession accessJwt refreshJwt
      atomically $ modifyTVar' sessions $ Map.insert (identifier login) session
      pure session
    _ -> error "Invalid credentials" -- In real impl, would use proper HTTP error

-- | Handle token refresh requests
refreshHandler :: BskyServerState -> Handler BskySession
refreshHandler BskyServerState {refreshCalls, jwtAudience, jwtSettings} = liftIO $ do
  -- Increment refresh counter
  atomicModifyIORef' refreshCalls (\n -> (n + 1, ()))

  -- Return new session (in real impl, would validate refresh token and extract DID)
  -- For now, use a placeholder DID
  let placeholderDid = "did:plc:placeholder"
  accessJwt <- createJWT jwtSettings placeholderDid jwtAudience "com.atproto.access"
  refreshJwt <- createJWT jwtSettings placeholderDid jwtAudience "com.atproto.refresh"
  pure $ BskySession accessJwt refreshJwt

-- | Handle createRecord requests
createRecordHandler :: BskyServerState -> BskyRecord BskyPost -> Handler Record
createRecordHandler BskyServerState {createdPosts} bskyRecord = liftIO $ do
  -- Store the post
  atomicModifyIORef' createdPosts (\posts -> (bskyRecord : posts, ()))

  -- Return a dummy record response
  pure $ Record "at://did:plc:example/app.bsky.feed.post/abc123" "bafyreicid123"

-- | Handle putRecord requests
putRecordHandler :: BskyServerState -> BskyRecord BskyPost -> Handler Record
putRecordHandler BskyServerState {updatedPosts} bskyRecord = liftIO $ do
  -- Store the updated post
  atomicModifyIORef' updatedPosts (\posts -> (bskyRecord : posts, ()))

  -- Return a dummy record response
  pure $ Record "at://did:plc:example/app.bsky.feed.post/abc123" "bafyreicid456"

-- | Handle listRecords requests
listRecordsHandler ::
  BskyServerState ->
  BskyHandle ->
  BskyType (Lexicon BskyPost) ->
  Maybe Int ->
  Maybe Text ->
  Maybe Bool ->
  Handler (ListRecordsResponse BskyPost)
listRecordsHandler _state _repo _collection _limit _cursor _reverse = do
  -- Return empty list for now (could be extended to return created posts)
  pure $ ListRecordsResponse Nothing []
