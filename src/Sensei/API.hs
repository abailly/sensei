{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.API
  ( SenseiAPI, senseiAPI,
    module Sensei.Flow, module Sensei.FlowView, module Sensei.Utils,
    GroupViews(..), Trace(..), Group(..), UserProfile(..),
    sameDayThan, mkGroupViewsBy, groupViews
  ) where

import Data.Aeson
    ( withText, FromJSON(parseJSON), Value(String), ToJSON(toJSON) )
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Time
    ( Day, LocalTime(..), UTCTime,
      TimeOfDay,
      NominalDiffTime,
      TimeZone )
import GHC.Generics ( Generic )
import Servant
    ( FromHttpApiData(parseUrlPiece),
      ToHttpApiData(toUrlPiece),
      type (:<|>),
      type (:>),
      ReqBody,
      JSON,
      Post,
      Capture,
      Get,
      QueryParams,
      Raw,
      Proxy(..) )
import System.Exit ( ExitCode(..) )
import Sensei.FlowView
import Sensei.Utils
import Sensei.Flow
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)

-- * API

type SenseiAPI =
  "trace" :> ReqBody '[JSON] Trace :> Post '[JSON] ()
    :<|> "flows"
      :> ( Capture "user" Text :> Capture "flowType" FlowType :> ReqBody '[JSON] FlowState :> Post '[JSON] ()
             :<|> Capture "user" Text :> "summary" :> Get '[JSON] [GroupViews (FlowType, NominalDiffTime)]
             :<|> Capture "user" Text :> Capture "day" Day :> "summary" :> Get '[JSON] [(FlowType, NominalDiffTime)]
             :<|> Capture "user" Text :> Capture "day" Day :> "notes" :>  Get '[JSON] [(LocalTime, Text.Text)]
             :<|> Capture "user" Text :> Capture "day" Day :> Get '[JSON] [FlowView]
             :<|> Capture "user" Text :> QueryParams "group" Group :> Get '[JSON] [GroupViews FlowView]
         )
    :<|> "users" :> (Capture "user" Text :> Get '[JSON] UserProfile)
    :<|> Raw

-- | Execution "trace" of a program
data Trace = Trace
  { timestamp :: UTCTime,
    directory :: FilePath,
    process :: Text,
    args :: [Text],
    exit_code :: ExitCode,
    elapsed :: NominalDiffTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

deriving instance ToJSON ExitCode

deriving instance FromJSON ExitCode

sameDayThan :: Day -> (a -> Day) -> a -> Bool
sameDayThan day selector a =
  selector a == day

senseiAPI :: Proxy SenseiAPI
senseiAPI = Proxy

-- | Grouping of `FlowView`
data Group = Day | Week | Month | Quarter | Year
  deriving (Eq, Read, Show, Ord, Generic, ToJSON, FromJSON)

instance ToHttpApiData Group where
  toUrlPiece f = Text.pack (show f)

instance FromHttpApiData Group where
  parseUrlPiece txt =
    case reads (Text.unpack txt) of
      ((g, _) : _) -> pure g
      _ -> Left $ "cannot parse group " <> txt

data GroupViews a
  = NoViews
  | Leaf {leafViews :: [a]}
  | GroupLevel {level :: Group, groupTime :: LocalTime, subGroup :: GroupViews a}
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Functor)

groupViews :: TimeOfDay -> TimeOfDay -> [Group] -> [FlowView] -> [GroupViews FlowView]
groupViews _ _ [] views = [Leaf views]
groupViews startOfDay endOfDay (Day : _groups) views =
  views
    |> NE.groupBy ((==) `on` (localDay . flowStart))
    |> mkGroupViewsBy startOfDay endOfDay Day
groupViews _ _ _ _ = error "unsupported group"

mkGroupViewsBy :: TimeOfDay -> TimeOfDay -> Group -> [NE.NonEmpty FlowView] -> [GroupViews FlowView]
mkGroupViewsBy startOfDay endOfDay Day =
  fmap mkGroup
  where
    normalized :: NE.NonEmpty FlowView -> [FlowView]
    normalized (view :| rest) =
      let viewDay = localDay (flowStart view)
       in normalizeViewsForDay (LocalTime viewDay startOfDay) (LocalTime viewDay endOfDay) (view : rest)

    mkGroup :: NE.NonEmpty FlowView -> GroupViews FlowView
    mkGroup (view :| rest) = GroupLevel Day (flowStart view) (Leaf (normalized (view :| rest)))
mkGroupViewsBy _ _ _ = error "unsupported group"

data UserProfile = UserProfile
  { userName :: Text,
    userTimezone :: TimeZone,
    userStartOfDay :: TimeOfDay,
    userEndOfDay :: TimeOfDay
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToJSON TimeZone where
  toJSON = String . Text.pack . iso8601Show

instance FromJSON TimeZone where
  parseJSON = withText "TimeZone" $ iso8601ParseM . Text.unpack
