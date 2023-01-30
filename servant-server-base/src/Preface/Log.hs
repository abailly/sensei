{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

-- | A simple logger that reads `LogEntry` from a `TBChan` and dumps JSON-formatted
-- strings to `stdout`.
module Preface.Log
  ( -- * Types
    LoggerEnv,
    module System.Log.FastLogger,

    -- * Constructor & Destructor
    withLogger,
    stopLogger,
    fakeLogger,
    loggerId,

    -- * Logging functions
    logInfo,
    logError,
    withLog,
  )
where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Exception.Safe (IOException, catch)
import Control.Monad (forever)
import Control.Monad.Trans (MonadIO (..))
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Time.Clock
  ( UTCTime,
    diffUTCTime,
    getCurrentTime,
  )
import System.IO (Handle, hFlush, stdout)
import System.Log.FastLogger

-- | Environment to control a logger thread.
data LoggerEnv = LoggerEnv
  { logger :: Maybe Logger,
    loggerId :: Text,
    logInfo :: forall a m. (MonadIO m, ToJSON a) => a -> m (),
    logError :: forall a m. (MonadIO m, ToJSON a) => a -> m (),
    withLog :: forall a b m. (MonadIO m, ToJSON a) => a -> m b -> m b,
    stopLogger :: forall m. MonadIO m => m ()
  }

type Logger = TBQueue BS.ByteString

fakeLogger :: LoggerEnv
fakeLogger = LoggerEnv Nothing "foo" (const $ pure ()) (const $ pure ()) (\_ -> id) (pure ())

-- | Bracket-style logger creation.
-- Takes a logger name and an action to run passing it the 'LoggerEnv' needed
-- to log stuff.
withLogger :: Text -> (LoggerEnv -> IO a) -> IO a
withLogger loggerId action = do
  queue <- newTBQueueIO 100
  let logger = Just queue
      logInfo a = logEvent' queue loggerId a
      logError a = logError' queue loggerId a
      withLog a act = withLog' queue loggerId a act
  withAsync (runLog queue stdout) $ \loggerThread -> do
    let env = LoggerEnv {stopLogger = liftIO (cancel loggerThread), ..}
    action env

runLog :: TBQueue BS.ByteString -> Handle -> IO a
runLog queue hdl =
  forever $ do
    toLog <- atomically $ readTBQueue queue
    -- ignore IOException in order to not kill the logging thread even if the output
    -- `Handle` is closed
    ( do
        BS.hPutStr hdl . (<> "\n") $ toLog
        hFlush hdl
      )
      `catch` \(_ :: IOException) -> pure ()

logEvent' :: (ToJSON a, MonadIO m) => Logger -> Text -> a -> m ()
logEvent' queue logId message = liftIO $ do
  ts <- getCurrentTime
  tid <- myThreadId
  let toLog =
        LBS.toStrict $
          encode $
            object
              [ "timestamp" .= ts,
                "loggerId" .= logId,
                "threadId" .= show tid,
                "message" .= message
              ]
  atomically $ writeTBQueue queue toLog

withLog' :: (ToJSON a, MonadIO m) => Logger -> Text -> a -> m b -> m b
withLog' queue logId message act = do
  startTime <- liftIO getCurrentTime
  logStart queue logId startTime message
  b <- act
  endTime <- liftIO getCurrentTime
  logEnd queue logId startTime endTime message
  pure b

logStart :: (ToJSON a, MonadIO m) => Logger -> Text -> UTCTime -> a -> m ()
logStart queue logId ts command = liftIO $ do
  tid <- myThreadId
  atomically $
    writeTBQueue queue $
      LBS.toStrict $
        encode $
          object
            [ "timestamp" .= ts,
              "loggerId" .= logId,
              "threadId" .= show tid,
              "message" .= command
            ]

logEnd :: (ToJSON a, MonadIO m) => Logger -> Text -> UTCTime -> UTCTime -> a -> m ()
logEnd queue logId ts en outcome = liftIO $ do
  tid <- myThreadId
  atomically $
    writeTBQueue queue $
      LBS.toStrict $
        encode $
          object
            [ "timestamp" .= en,
              "loggerId" .= logId,
              "threadId" .= show tid,
              "durationMs" .= ((diffUTCTime en ts) * 1000),
              "message" .= outcome
            ]

logError' :: (ToJSON a, MonadIO m) => Logger -> Text -> a -> m ()
logError' queue logId err = liftIO $ do
  ts <- getCurrentTime
  tid <- myThreadId
  atomically $
    writeTBQueue queue $
      LBS.toStrict $
        encode $
          object
            [ "timestamp" .= ts,
              "loggerId" .= logId,
              "threadId" .= show tid,
              "error" .= err
            ]
