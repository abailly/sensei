{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sensei.DB (
  DB (..), Event(..), flowView, flowViewBuilder, notesViewBuilder, commandViewBuilder
  ) where

import Data.Text(Text)
import Data.Time
import Sensei.API
import Data.Aeson (ToJSON(..), FromJSON(..))
import Control.Applicative (Alternative((<|>)))

-- | Interface to the underlying database.
-- This interface provide high-level functions to retrieve
-- and store various pieces of data for the `Server`-side operations.
class (Monad m) => DB m where

   -- | Initialises the connection, engine or whatever that underlies the DB operations
  initLogStorage :: m ()

  -- | Write a new `Trace` to the DB
  writeTrace :: Trace -> m ()

  -- | Write a new `Flow` to the DB
  writeFlow :: Flow -> m ()

  -- | Write user's profile to the DB
  writeProfile :: UserProfile -> m ()

  -- | Read all notes from DB
  --  The `UserProfile` is needed to convert timestamps to the user's local timezone
  readNotes :: UserProfile -> m [(LocalTime, Text)]

  -- | Read all flows and construct `FlowView` items from the DB
  --  The `UserProfile` is needed to convert timestamps to the user's local timezone
  readViews :: UserProfile -> m [FlowView]

  -- | Read all `Trace` from DB and construct appropriate `CommandView` from each of them
  readCommands :: UserProfile -> m [CommandView]

  -- | Read the user's profile
  -- This function may fail of there's no profile or the format is incorrect
  readProfile :: m (Either Text UserProfile)

-- | Common type grouping all kind of events that are stored in the DB
-- TODO: This type is in an early stage and only used currently when migrating
-- database, refactor when exposing the full log to the user. In particular
-- the `Flow` and `Trace` types should be unified.
data Event =
  F Flow | T Trace
  deriving (Eq, Show)

instance ToJSON Event where
  toJSON (F flow) = toJSON flow
  toJSON (T trace) = toJSON trace

instance FromJSON Event where
  parseJSON v = T <$> parseJSON v <|> F <$> parseJSON v

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

-- |Basically a combination of a `filter` and a single step of a fold
-- Should be refactored to something more standard
flowView :: Flow -> Text -> (Flow -> [a] -> [a]) -> [a] -> [a]
flowView f@Flow {..} usr mkView views =
  if _flowUser _flowState == usr
    then mkView f views
    else views
