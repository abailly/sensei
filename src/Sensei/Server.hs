{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Sensei.Server where

import Control.Concurrent.MVar
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Aeson hiding (Options)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.Text (Text)
import Data.Time
import Sensei.API
import Sensei.IO
import Servant
import System.IO
import Control.Monad.Except


killS ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) => MVar () -> m ()
killS signal = liftIO (putMVar signal ())

traceS ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) => FilePath -> Trace -> m ()
traceS file trace = liftIO $
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ encode trace <> "\n"
    hFlush out

flowS ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) => FilePath -> Text -> FlowType -> FlowState -> m ()
flowS file _ flowTyp flow = liftIO $
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ encode (Flow flowTyp flow currentVersion) <> "\n"
    hFlush out

notesDayS ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) => FilePath -> Text -> Day -> m [NoteView]
notesDayS file usr day = do
  usrProfile <- getUserProfileS usr
  notes <- liftIO $ readNotes file usrProfile
  pure $ map (uncurry NoteView) $ filter (sameDayThan day (localDay . fst)) notes

commandsDayS ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) => FilePath -> Text -> Day -> m [CommandView]
commandsDayS file usr day = do
  usrProfile <- getUserProfileS usr
  commands <- liftIO $ readCommands file usrProfile
  pure $ filter (commandOnDay day) commands

queryFlowDayS ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) => FilePath -> Text -> Day -> m [FlowView]
queryFlowDayS file usr day = do
  usrProfile <- getUserProfileS usr
  views <- liftIO $ readViews file usrProfile
  pure $ filter (flowOnDay day) views

queryFlowDaySummaryS ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) => FilePath -> Text -> Day -> m FlowSummary
queryFlowDaySummaryS file usr day = do
  usrProfile <- getUserProfileS usr
  views <- liftIO $ readViews file usrProfile
  commands <- liftIO $ readCommands file usrProfile
  let summaryFlows = views
        |> filter (flowOnDay day)
        |> summarize
      summaryCommands = commands
        |> filter (commandOnDay day)
        |> summarize
      summaryPeriod = (day, day)
  pure $ FlowSummary{..}


queryFlowSummaryS ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) => FilePath -> Text -> m [GroupViews (FlowType, NominalDiffTime)]
queryFlowSummaryS file usr = do
  usrProfile@UserProfile {userStartOfDay, userEndOfDay} <- getUserProfileS usr
  views <- liftIO $ groupViews userStartOfDay userEndOfDay [Day] <$> readViews file usrProfile
  pure $ views |> fmap summary
  where
    summary :: GroupViews FlowView -> GroupViews (FlowType, NominalDiffTime)
    summary grp =
      ( case grp of
          NoViews -> NoViews
          (Leaf []) -> Leaf []
          (Leaf vs) -> Leaf (summarize vs)
          (GroupLevel g u gf) -> GroupLevel g u (summary gf)
      )

-- summarize flows@(f NE.:| _) = (flowType f, sum $ fmap duration flows)

queryFlowS ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) => FilePath -> Text -> [Group] -> m [GroupViews FlowView]
queryFlowS file usr groups = do
  usrProfile@UserProfile {userStartOfDay, userEndOfDay} <- getUserProfileS usr
  liftIO $ groupViews userStartOfDay userEndOfDay (List.sort groups) <$> readViews file usrProfile

getUserProfileS ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) => Text -> m UserProfile
getUserProfileS _ = do
  configDir <- ask
  prof <- liftIO $ readProfile configDir
  case prof of
    Left _ -> pure defaultProfile
    Right prf -> pure prf

putUserProfileS ::
  (MonadReader FilePath m, MonadIO m, MonadError ServerError m) => Text -> UserProfile -> m NoContent
putUserProfileS  _ profile = do
  configDir <- ask
  liftIO $ writeProfile configDir profile
  pure NoContent
