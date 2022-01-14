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
    toNoteView,
    commandViewBuilder,
  )
where

import Control.Exception.Safe
import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)
import Preface.Codec (Encoded, Hex)
import Sensei.API
import Sensei.Time

data Pagination
  = Page {pageNumber :: Natural, pageSize :: Natural}
  | NoPagination
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EventsQueryResult = EventsQueryResult
  { resultEvents :: [EventView],
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
class (Exception (DBError m), Eq (DBError m), MonadCatch m) => DB m where
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

  -- | Read a single `Flow` from the storage, pointed at by given `Reference`.
  readFlow :: UserProfile -> Reference -> m (Maybe EventView)

  -- | Read raw events stored in the database, younger events first.
  readEvents :: UserProfile -> Pagination -> m EventsQueryResult

  -- | Read all notes from DB
  --  The `UserProfile` is needed to convert timestamps to the user's local timezone
  readNotes :: UserProfile -> TimeRange -> m [NoteView]

  -- | Read all goal operations from DB
  readGoals :: UserProfile -> m [GoalOp]

  -- | Full-text search of notes
  searchNotes :: UserProfile -> Text -> m [NoteView]

  -- | Read all flows and construct `FlowView` items from the DB
  --  The `UserProfile` is needed to convert timestamps to the user's local timezone
  readViews :: UserProfile -> m [FlowView]

  -- | Read all `Trace` from DB and construct appropriate `CommandView` from each of them
  readCommands :: UserProfile -> m [CommandView]

  -- | Read a user's profile
  -- This function may fail of there's no profile or the format is incorrect.
  readProfile :: Text -> m UserProfile

  -- | Write an existing user's profile to the DB
  writeProfile :: UserProfile -> m ()

  -- | Create a new user profile to the DB, assuming it does not
  -- already exist.
  insertProfile :: UserProfile -> m (Encoded Hex)

flowViewBuilder :: Text -> TimeZone -> TimeOfDay -> ProjectsMap -> EventView -> [FlowView] -> [FlowView]
flowViewBuilder userName userTimezone userEndOfDay projectsMap flow =
  flowView flow userName (appendFlow userTimezone userEndOfDay projectsMap)

notesViewBuilder :: Text -> TimeZone -> ProjectsMap -> EventView -> [NoteView] -> [NoteView]
notesViewBuilder userName userTimezone projectsMap flow = flowView flow userName f
  where
    f :: EventView -> [NoteView] -> [NoteView]
    f EventView {event = (EventNote note)} fragments =
      toNoteView userTimezone projectsMap note : fragments
    f _ fragments = fragments

toNoteView :: TimeZone -> ProjectsMap -> NoteFlow -> NoteView
toNoteView userTimezone projectsMap note =
  NoteView
    { noteStart = utcToLocalTime userTimezone (note ^. noteTimestamp),
      noteView = note ^. noteContent,
      noteProject = projectsMap `selectProject` (note ^. noteDir),
      noteTags = []
    }

commandViewBuilder :: TimeZone -> ProjectsMap -> EventView -> [CommandView] -> [CommandView]
commandViewBuilder userTimezone projectsMap EventView {event = t@(EventTrace _)} acc = fromJust (mkCommandView userTimezone projectsMap t) : acc
commandViewBuilder _ _ _ acc = acc

-- | Basically a combination of a `filter` and a single step of a fold
--  Should be refactored to something more standard
flowView :: EventView -> Text -> (EventView -> [a] -> [a]) -> [a] -> [a]
flowView e usr mkView views =
  if eventUser (event e) == usr
    then mkView e views
    else views
