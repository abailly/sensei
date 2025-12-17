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
module Sensei.Bsky.Server (
    -- * Server Application
    bskyTestApp,
    bskyTestAppWithState,

    -- * Server State
    BskyServerState (..),
    newBskyServerState,

    -- * API Implementation
    BskyTestAPI,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Network.Wai (Application)
import Sensei.Bsky
    ( BskyPost,
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
import Servant
    ( Handler,
      Proxy (..),
      serve,
      type (:<|>) (..),
      type (:>),
    )

-- * API Definition

-- | Subset of Bluesky API that Sensei uses
type BskyTestAPI =
    "xrpc"
        :> ( Login
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
      -- | Default session to return on login
      defaultSession :: BskySession
    }

-- | Create new server state with default values
newBskyServerState :: BskySession -> IO BskyServerState
newBskyServerState defaultSession = do
    sessions <- newTVarIO Map.empty
    createdPosts <- newIORef []
    updatedPosts <- newIORef []
    loginCalls <- newIORef 0
    refreshCalls <- newIORef 0
    pure BskyServerState {..}

-- * Server Implementation

-- | Create a WAI Application for the Bsky test server with default state
bskyTestApp :: BskySession -> IO Application
bskyTestApp defaultSession = do
    state <- newBskyServerState defaultSession
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
loginHandler BskyServerState {sessions, loginCalls, defaultSession} login = liftIO $ do
    -- Increment login counter
    atomicModifyIORef' loginCalls (\n -> (n + 1, ()))

    -- Store session for this user
    let session = defaultSession
    atomically $ modifyTVar' sessions $ Map.insert (identifier login) session

    pure session

-- | Handle token refresh requests
refreshHandler :: BskyServerState -> Handler BskySession
refreshHandler BskyServerState {refreshCalls, defaultSession} = liftIO $ do
    -- Increment refresh counter
    atomicModifyIORef' refreshCalls (\n -> (n + 1, ()))

    -- Return new session (in real impl, would validate refresh token)
    pure defaultSession

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
