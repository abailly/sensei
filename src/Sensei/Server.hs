{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.Server where

import Control.Concurrent.MVar
import Control.Exception.Safe (throwM)
import Control.Monad (join)
import Control.Monad.Trans
import qualified Data.List as List
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Network.HTTP.Link as Link
import Network.URI.Extra ()
import Sensei.API
import Sensei.DB
import Sensei.Server.Auth.Types (Credentials (..), authenticateUser)
import Sensei.Server.Links (nextDayLink, nextPageLink, periodLinks, previousDayLink, previousPageLink)
import Sensei.Time hiding (getCurrentTime)
import Sensei.Version (Versions (..), senseiVersion)
import Servant
import Servant.Auth.Server as SAS

killS ::
  MonadIO m => MVar () -> m ()
killS signal = liftIO (putMVar signal ())

setCurrentTimeS ::
  DB m => Text -> Timestamp -> m ()
setCurrentTimeS usr Timestamp {timestamp} = do
  usrProfile <- getUserProfileS usr
  setCurrentTime usrProfile timestamp

getCurrentTimeS ::
  DB m => Text -> m Timestamp
getCurrentTimeS usr = do
  usrProfile <- getUserProfileS usr
  Timestamp <$> getCurrentTime usrProfile

postEventS ::
  (DB m) => [Event] -> m ()
postEventS = mapM_ writeEvent

updateFlowStartTimeS ::
  (DB m) => Text -> TimeDifference -> m Event
updateFlowStartTimeS _ timediff =
  updateLatestFlow (toNominalDiffTime timediff)

notesDayS ::
  (DB m) => Text -> Day -> m (Headers '[Header "Link" Text] [NoteView])
notesDayS usr day = do
  usrProfile <- getUserProfileS usr
  notes <- readNotes usrProfile (rangeFromDay day (userTimezone usrProfile))
  let hdrs =
        addHeader
          ( writeLinkHeader $
              catMaybes $
                [ nextDayLink usr (Just day),
                  previousDayLink usr (Just day)
                ]
          )
  pure $ hdrs $ map (uncurry NoteView) notes

searchNoteS ::
  (DB m) => Text -> Maybe Text -> m [NoteView]
searchNoteS _ Nothing = pure []
searchNoteS usr (Just search) = do
  usrProfile <- getUserProfileS usr
  rawNotes <- searchNotes usrProfile search
  pure $ fmap (uncurry NoteView) rawNotes

commandsDayS ::
  (DB m) => Text -> Day -> m [CommandView]
commandsDayS usr day = do
  usrProfile <- getUserProfileS usr
  commands <- readCommands usrProfile
  pure $ filter (commandInPeriod (Just $ LocalTime day midnight) (Just $ LocalTime (succ day) midnight)) commands

queryFlowDayS ::
  (DB m) => Text -> Day -> m [FlowView]
queryFlowDayS usr day = do
  usrProfile <- getUserProfileS usr
  views <- readViews usrProfile
  pure $ filter (flowInPeriod (Just $ LocalTime day midnight) (Just $ LocalTime (succ day) midnight)) views

queryFlowPeriodSummaryS ::
  (DB m) => Text -> Maybe Day -> Maybe Day -> Maybe Group -> m (Headers '[Header "Link" Text] FlowSummary)
queryFlowPeriodSummaryS usr fromDay toDay period = do
  let fromTime = flip LocalTime midnight <$> fromDay
      toTime = flip LocalTime midnight <$> toDay
  usrProfile@UserProfile {userName} <- getUserProfileS usr
  result <- makeSummary fromTime toTime <$> readViews usrProfile <*> readCommands usrProfile
  let links = addHeader . writeLinkHeader <$> (join $ periodLinks userName <$> fromDay <*> toDay <*> period)
  pure $ fromMaybe noHeader links $ result

getFlowS ::
  (DB m) => Text -> Reference -> m (Maybe Event)
getFlowS usr ref = do
  profile <- getUserProfileS usr
  readFlow profile ref

queryFlowS ::
  (DB m) => Text -> [Group] -> m [GroupViews FlowView]
queryFlowS usr groups = do
  usrProfile@UserProfile {userStartOfDay, userEndOfDay} <- getUserProfileS usr
  groupViews userStartOfDay userEndOfDay (List.sort groups) <$> readViews usrProfile

getLogS ::
  DB m => Text -> Maybe Natural -> m (Headers '[Header "Link" Text] [Event])
getLogS userName page = do
  usrProfile <- getUserProfileS userName
  EventsQueryResult {..} <- readEvents usrProfile (maybe NoPagination (\p -> Page p 50) page)
  let nextHeader =
        if endIndex < totalEvents
          then nextPageLink userName page
          else Nothing
      previousHeader =
        if startIndex > 1
          then previousPageLink userName page
          else Nothing
      links =
        case catMaybes [nextHeader, previousHeader] of
          [] -> noHeader
          ls -> addHeader $ writeLinkHeader ls
  pure $ links resultEvents

getUserProfileS ::
  (DB m) => Text -> m UserProfile
getUserProfileS userName = do
  prof <- readProfile userName
  case prof of
    Left _ -> pure defaultProfile
    Right prf -> pure prf

putUserProfileS ::
  (DB m) => Text -> UserProfile -> m NoContent
putUserProfileS _ profile = do
  writeProfile profile
  pure NoContent

getVersionsS ::
  (Monad m) => m Versions
getVersionsS = pure $ Versions senseiVersion senseiVersion currentVersion currentVersion

loginS ::
  (MonadIO m, DB m) =>
  JWTSettings ->
  CookieSettings ->
  Credentials ->
  m
    ( Headers
        '[ Header "Set-Cookie" SetCookie,
           Header "Set-Cookie" SetCookie
         ]
        UserProfile
    )
loginS js cs Credentials {credLogin, credPassword} = do
  profile <- getUserProfileS credLogin
  case authenticateUser credPassword profile of
    SAS.Authenticated usr -> do
      mApplyCookies <- liftIO $ acceptLogin cs js usr
      case mApplyCookies of
        Nothing -> throwM err401 {errBody = "Failed to generate cookies for user login"}
        Just applyCookies -> return $ applyCookies profile
    _ -> throwM err401 {errBody = "Fail to authenticate user"}
