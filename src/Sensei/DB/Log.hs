{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Log all DB actions to a given `LoggerEnv`.
module Sensei.DB.Log where

import Control.Monad.Reader
import Data.Aeson (ToJSON)
import Data.Aeson.Extra (FromJSON)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import GHC.Generics (Generic)
import Preface.Codec
import Preface.Log
import Sensei.API
import Sensei.DB

data DBLog
  = InitLogStorage
  | SetCurrentTime {user :: Text, setTime :: UTCTime}
  | GetCurrentTime {user :: Text}
  | WriteEvent {event :: Event}
  | UpdateLatestFlow {setTimeShift :: NominalDiffTime}
  | ReadFlow {user :: Text, reference :: Reference}
  | ReadEvents {user :: Text, pagination :: Pagination}
  | ReadNotes {user :: Text, timeRange :: TimeRange}
  | ReadGoals {user :: Text}
  | SearchNotes {user :: Text, search :: Text}
  | ReadViews {user :: Text}
  | ReadCommands {user :: Text}
  | ReadProfile {user :: Text}
  | ReadProfileById {uid :: Encoded Hex}
  | WriteProfile {user :: Text}
  | InsertProfile {profile :: UserProfile}
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (DB m, MonadIO m) => DB (ReaderT LoggerEnv m) where
  type DBError (ReaderT LoggerEnv m) = DBError m
  initLogStorage =
    ReaderT $ \l -> withLog l InitLogStorage initLogStorage
  setCurrentTime u ts =
    ReaderT $ \l -> withLog l (SetCurrentTime (userName u) ts) (setCurrentTime u ts)
  getCurrentTime u =
    ReaderT $ \l -> withLog l (GetCurrentTime (userName u)) (getCurrentTime u)
  writeEvent t =
    ReaderT $ \l -> withLog l (WriteEvent t) (writeEvent t)
  updateLatestFlow ts =
    ReaderT $ \l -> withLog l (UpdateLatestFlow ts) (updateLatestFlow ts)
  writeProfile u =
    ReaderT $ \l -> withLog l (WriteProfile (userName u)) (writeProfile u)
  readFlow u r =
    ReaderT $ \l -> withLog l (ReadFlow (userName u) r) (readFlow u r)
  readEvents u p =
    ReaderT $ \l -> withLog l (ReadEvents (userName u) p) (readEvents u p)
  readNotes u rge =
    ReaderT $ \l -> withLog l (ReadNotes (userName u) rge) (readNotes u rge)
  readGoals u =
    ReaderT $ \l -> withLog l (ReadGoals (userName u)) (readGoals u)
  searchNotes u txt =
    ReaderT $ \l -> withLog l (SearchNotes (userName u) txt) (searchNotes u txt)
  readViews u =
    ReaderT $ \l -> withLog l (ReadViews (userName u)) (readViews u)
  readCommands u =
    ReaderT $ \l -> withLog l (ReadCommands (userName u)) (readCommands u)
  readProfile n =
    ReaderT $ \l -> withLog l (ReadProfile n) (readProfile n)
  readProfileById u =
    ReaderT $ \l -> withLog l (ReadProfileById u) (readProfileById u)
  insertProfile p =
    ReaderT $ \l -> withLog l (InsertProfile p) (insertProfile p)
