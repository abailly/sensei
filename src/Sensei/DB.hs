{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.Exception.Safe
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)
import Sensei.API
import Sensei.Time

data Pagination = Page {pageNumber :: Natural, pageSize :: Natural}
  | NoPagination
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EventsQueryResult = EventsQueryResult
  { resultEvents :: [Event],
    eventsCount :: Natural,
    startIndex :: Natural,
    endIndex :: Natural,
    totalEvents :: Natural
  }
  deriving (Eq, Show)

-- | Interface to the underlying database.
-- This interface provide high-level functions to retrieve
-- and store various pieces of data for the `Server`-side operations. It is expected
-- to throw exceptions of type `DBError m`.
class (Exception (DBError m), MonadCatch m) => DB m where
  type DBError m :: *

  -- | Stores the current timestamp
  -- This is only used for development or testing purpose
  setCurrentTime :: UserProfile -> UTCTime -> m ()

  -- | Retrieves the current timestamp
  -- This is only used for development or testing purpose
  getCurrentTime :: UserProfile -> m UTCTime

  -- | Initialises the connection, engine or whatever that underlies the DB operations.
  initLogStorage :: m ()

  -- | Write a new `Event` to the DB
  writeEvent :: Event -> m ()

  -- | Update the latest's flow start time by given time difference.
  updateLatestFlow :: NominalDiffTime -> m Event

  -- | Write user's profile to the DB
  writeProfile :: UserProfile -> m ()

  -- | Read a single `Flow` from the storage, pointed at by given `Reference`.
  readFlow :: UserProfile -> Reference -> m (Maybe Event)

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

  -- | Read a user's profile
  -- This function may fail of there's no profile or the format is incorrect.
  readProfile :: Text -> m (Either Text UserProfile)

flowViewBuilder :: Text -> TimeZone -> TimeOfDay -> Event -> [FlowView] -> [FlowView]
flowViewBuilder userName userTimezone userEndOfDay flow =
  flowView flow userName (appendFlow userTimezone userEndOfDay)

-- | Basically a combination of a `filter` and a single step of a fold
--  Should be refactored to something more standard
flowView :: Event -> Text -> (Event -> [a] -> [a]) -> [a] -> [a]
flowView e usr mkView views =
  if eventUser e == usr
    then mkView e views
    else views

notesViewBuilder :: Text -> TimeZone -> Event -> [(LocalTime, Text)] -> [(LocalTime, Text)]
notesViewBuilder userName userTimezone flow = flowView flow userName f
  where
    f :: Event -> [(LocalTime, Text)] -> [(LocalTime, Text)]
    f (EventNote (NoteFlow _ st _ note)) fragments =
      (utcToLocalTime userTimezone st, note) : fragments
    f _ fragments = fragments

commandViewBuilder :: TimeZone -> Event -> [CommandView] -> [CommandView]
commandViewBuilder userTimezone t@(EventTrace _) acc = fromJust (mkCommandView userTimezone t) : acc
commandViewBuilder _ _ acc = acc
