{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.DB
  ( DB (..),
    Event (..),
    Reference (..),
    Pagination (..),
    EventsQueryResult (..),
    TimeRange (..),
    inRange,
    flowView,
    flowViewBuilder,
    notesViewBuilder,
    commandViewBuilder,
  )
where

import Data.Text (Text)
import Data.Time
import Sensei.API
import Sensei.Time

data Pagination = Page {pageNumber :: Natural, pageSize :: Natural}
  deriving (Eq, Show)

data EventsQueryResult = EventsQueryResult
  { events :: [Event],
    eventsCount :: Natural,
    startIndex :: Natural,
    endIndex :: Natural,
    totalEvents :: Natural
  }
  deriving (Eq, Show)

-- | Interface to the underlying database.
-- This interface provide high-level functions to retrieve
-- and store various pieces of data for the `Server`-side operations.
class (Monad m) => DB m where
  -- | Stores the current timestamp
  -- This is only used for development or testing purpose
  setCurrentTime :: UserProfile -> UTCTime -> m ()

  -- | Retrieves the current timestamp
  -- This is only used for development or testing purpose
  getCurrentTime :: UserProfile -> m UTCTime

  -- | Initialises the connection, engine or whatever that underlies the DB operations.
  initLogStorage :: m ()

  -- | Write a new `Trace` to the DB
  writeTrace :: Trace -> m ()

  -- | Write a new `Flow` to the DB
  writeFlow :: Flow -> m ()

  -- | Update the latest's flow start time by given time difference.
  updateLatestFlow :: NominalDiffTime -> m FlowState

  -- | Write user's profile to the DB
  writeProfile :: UserProfile -> m ()

  -- | Read a single `Flow` from the storage, pointed at by given `Reference`.
  readFlow :: UserProfile -> Reference -> m (Maybe Flow)

  -- | Read raw events stored in the database, younger events first.
  readEvents :: UserProfile -> Pagination -> m EventsQueryResult

  -- | Read all notes from DB
  --  The `UserProfile` is needed to convert timestamps to the user's local timezone
  readNotes :: UserProfile -> TimeRange -> m [(LocalTime, Text)]

  -- | Full-text search of notes
  searchNotes :: UserProfile -> Text -> m [(LocalTime, Text)]

  -- | Read all flows and construct `FlowView` items from the DB
  --  The `UserProfile` is needed to convert timestamps to the user's local timezone
  readViews :: UserProfile -> m [FlowView]

  -- | Read all `Trace` from DB and construct appropriate `CommandView` from each of them
  readCommands :: UserProfile -> m [CommandView]

  -- | Read the user's profile
  -- This function may fail of there's no profile or the format is incorrect
  readProfile :: m (Either Text UserProfile)

flowViewBuilder :: Text -> TimeZone -> TimeOfDay -> Flow -> [FlowView] -> [FlowView]
flowViewBuilder userName userTimezone userEndOfDay flow =
  flowView flow userName (appendFlow userTimezone userEndOfDay)

notesViewBuilder :: Text -> TimeZone -> Flow -> [(LocalTime, Text)] -> [(LocalTime, Text)]
notesViewBuilder userName userTimezone flow = flowView flow userName f
  where
    f :: Flow -> [(LocalTime, Text)] -> [(LocalTime, Text)]
    f (Flow Note (FlowNote _ st _ note) _) fragments =
      (utcToLocalTime userTimezone st, note) : fragments
    f _ fragments = fragments

commandViewBuilder :: TimeZone -> Trace -> [CommandView] -> [CommandView]
commandViewBuilder userTimezone t acc = mkCommandView userTimezone t : acc

-- | Basically a combination of a `filter` and a single step of a fold
--  Should be refactored to something more standard
flowView :: Flow -> Text -> (Flow -> [a] -> [a]) -> [a] -> [a]
flowView f@Flow {..} usr mkView views =
  if _flowUser _flowState == usr
    then mkView f views
    else views
