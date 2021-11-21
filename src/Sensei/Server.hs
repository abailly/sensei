{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.Server (
    -- * Endpoints
    killS,
    setCurrentTimeS,
    getCurrentTimeS,
    updateFlowStartTimeS,
    postEventS,
    notesDayS,
    searchNoteS,
    commandsDayS,
    queryFlowDayS,
    queryFlowPeriodSummaryS,
    getFlowS,
    queryFlowS,
    getLogS,
    getUserProfileS,
    getUserProfileIdS,
    putUserProfileS,
    createUserProfileS,
    getFreshTokenS,
    getVersionsS,
    postGoalS,
    getGoalsS,
    loginS,
    module Sensei.Server.OpenApi,

    -- * Links
    module Sensei.Server.Links,

    -- * Configuration
    module Sensei.Server.Config,
    module Sensei.Server.Options,

    -- * Authentication
    module Sensei.Server.Auth,

    -- * Server-side HTML
    module Sensei.Server.UI,
    logoutS,
) where

import Control.Concurrent.MVar (MVar, putMVar)
import Control.Exception.Safe (throwM, try)
import Control.Monad (join)
import Control.Monad.Trans (MonadIO (..))
import qualified Data.List as List
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.URI.Extra ()
import Preface.Codec (Encoded, Hex)
import Sensei.API (
    CommandView,
    CurrentGoals (CurrentGoals),
    Day,
    Event (EventGoal),
    EventView,
    FlowSummary,
    FlowView,
    GoalOp,
    Goals (current),
    Group,
    GroupViews,
    LocalTime (LocalTime),
    Natural,
    NoteView,
    Reference,
    TimeDifference,
    Timestamp (..),
    UserName,
    UserProfile (..),
    commandInPeriod,
    flowInPeriod,
    groupViews,
    makeGoals,
    makeSummary,
    midnight,
    rangeFromDay,
    toNominalDiffTime,
 )
import Sensei.DB (
    DB (..),
    EventsQueryResult (..),
    Pagination (NoPagination, Page),
 )
import Sensei.Server.Auth
import Sensei.Server.Config
import Sensei.Server.Links
import Sensei.Server.OpenApi
import Sensei.Server.Options
import Sensei.Server.UI
import Sensei.Time ()
import Sensei.Version (Versions (..), currentVersion, senseiVersion)
import Servant (
    Header,
    Headers,
    NoContent (..),
    ServerError (errBody),
    addHeader,
    err400,
    err401,
    noHeader,
 )
import Servant.Auth.Server as SAS (AuthResult (Authenticated))

killS ::
    MonadIO m => MVar () -> m ()
killS signal = liftIO (putMVar signal ())

setCurrentTimeS ::
    DB m => Text -> Timestamp -> m ()
setCurrentTimeS usr Timestamp{timestamp} = do
    usrProfile <- getUserProfileS usr
    setCurrentTime usrProfile timestamp

getCurrentTimeS ::
    DB m => Text -> m Timestamp
getCurrentTimeS usr = do
    usrProfile <- getUserProfileS usr
    Timestamp <$> getCurrentTime usrProfile

postEventS ::
    (DB m) => UserName -> [Event] -> m ()
postEventS _user = mapM_ writeEvent

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
                    catMaybes
                        [ nextDayLink usr (Just day)
                        , previousDayLink usr (Just day)
                        ]
                )
    pure $ hdrs notes

searchNoteS ::
    (DB m) => Text -> Maybe Text -> m [NoteView]
searchNoteS _ Nothing = pure []
searchNoteS usr (Just search) = do
    usrProfile <- getUserProfileS usr
    searchNotes usrProfile search

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
    usrProfile@UserProfile{userName} <- getUserProfileS usr
    result <- makeSummary fromTime toTime <$> readViews usrProfile <*> readCommands usrProfile
    let links = addHeader . writeLinkHeader <$> join (periodLinks userName <$> fromDay <*> toDay <*> period)
    pure $ fromMaybe noHeader links result

getFlowS ::
    (DB m) => Text -> Reference -> m (Maybe EventView)
getFlowS usr ref = do
    profile <- getUserProfileS usr
    readFlow profile ref

queryFlowS ::
    (DB m) => Text -> [Group] -> m [GroupViews FlowView]
queryFlowS usr groups = do
    usrProfile@UserProfile{userStartOfDay, userEndOfDay} <- getUserProfileS usr
    groupViews userStartOfDay userEndOfDay (List.sort groups) <$> readViews usrProfile

getLogS ::
    DB m => Text -> Maybe Natural -> m (Headers '[Header "Link" Text] [EventView])
getLogS userName page = do
    usrProfile <- getUserProfileS userName
    EventsQueryResult{..} <- readEvents usrProfile (maybe NoPagination (`Page` 50) page)
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

createUserProfileS ::
    forall m. (DB m) => UserProfile -> m (Encoded Hex)
createUserProfileS u = do
    result <- try @_ @(DBError m) $ insertProfile u
    case result of
        Left ignored -> throwM $ err400{errBody = encodeUtf8 $ pack $ show ignored}
        Right uid -> pure uid

getUserProfileS ::
    forall m. (DB m) => Text -> m UserProfile
getUserProfileS userName = do
    result <- try @_ @(DBError m) $ readProfile userName
    case result of
        Left e -> throwM $ err400{errBody = encodeUtf8 $ pack $ show e}
        Right p -> pure p

getUserProfileIdS ::
  forall m. (DB m) => Encoded Hex -> m UserProfile
getUserProfileIdS userId = do
  result <- try @_ @(DBError m) $ readProfileById userId
  case result of
    Left e -> throwM $ err400 {errBody = encodeUtf8 $ pack $ show e}
    Right p -> pure p

putUserProfileS ::
    (DB m) => Text -> UserProfile -> m NoContent
putUserProfileS user profile
    | user /= userName profile = throwM err400
    | otherwise = NoContent <$ writeProfile profile

getFreshTokenS ::
    (DB m, MonadIO m) =>
    JWTSettings ->
    Text ->
    m SerializedToken
getFreshTokenS js userName = do
    UserProfile{userId} <- getUserProfileS userName
    liftIO $ makeToken js (AuthToken userId 1)

getVersionsS ::
    (Monad m) => m Versions
getVersionsS = pure $ Versions senseiVersion senseiVersion currentVersion currentVersion

postGoalS :: DB m => Text -> GoalOp -> m CurrentGoals
postGoalS userName op = do
    writeEvent (EventGoal op)
    CurrentGoals . current <$> getGoalsS userName

getGoalsS :: DB m => Text -> m Goals
getGoalsS userName = do
    profile <- getUserProfileS userName
    makeGoals <$> readGoals profile

loginS ::
    (MonadIO m, DB m) =>
    JWTSettings ->
    CookieSettings ->
    Credentials ->
    m
        ( Headers
            '[ Header "Set-Cookie" SetCookie
             , Header "Set-Cookie" SetCookie
             ]
            UserProfile
        )
loginS js cs Credentials{credLogin, credPassword} = do
    profile <- getUserProfileS credLogin
    case authenticateUser credPassword profile of
        SAS.Authenticated usr -> do
            mApplyCookies <- liftIO $ acceptLogin cs js usr
            case mApplyCookies of
                Nothing -> throwM err401{errBody = "Failed to generate cookies for user login"}
                Just applyCookies -> return $ applyCookies profile
        _ -> throwM err401{errBody = "Fail to authenticate user"}

logoutS ::
    (MonadIO m, DB m) =>
    CookieSettings ->
    m
        ( Headers
            '[ Header "Set-Cookie" SetCookie
             , Header "Set-Cookie" SetCookie
             ]
            NoContent
        )
logoutS cs = pure $ clearSession cs NoContent
