{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sensei.DB (DB (..)) where

import Data.Text(Text)
import Data.Time
import Sensei.API

-- | Interface to the underlying database.
-- This interface provide high-level functions to retrieve
-- and store various pieces of data for the `Server`-side operations.
class (Monad m) => DB m where

   -- | Initialises the connection/storage/engine/whatever that underlies the DB operations
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
