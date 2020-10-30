{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.API where

import Data.Aeson hiding (Options)
import Data.Function (on, (&))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Data.Time
import GHC.Generics
import Servant
import System.Exit

-- * API

type SenseiAPI =
  "trace" :> ReqBody '[JSON] Trace :> Post '[JSON] ()
    :<|> "flows"
      :> ( Capture "flowType" FlowType :> ReqBody '[JSON] FlowState :> Post '[JSON] ()
             :<|> Capture "user" String :> Capture "day" Day :> "summary" :> Get '[JSON] [(FlowType, NominalDiffTime)]
             :<|> Capture "user" String :> Capture "day" Day :> "notes" :> Get '[JSON] [(UTCTime, Text.Text)]
             :<|> Capture "user" String :> Capture "day" Day :> Get '[JSON] [FlowView]
             :<|> Capture "user" String :> QueryParams "group" Group :> Get '[JSON] [GroupViews]
         )
    :<|> Raw

-- | Execution "trace" of a program
data Trace = Trace
  { timestamp :: UTCTime,
    directory :: FilePath,
    process :: String,
    args :: [String],
    exit_code :: ExitCode,
    elapsed :: NominalDiffTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

deriving instance ToJSON ExitCode

deriving instance FromJSON ExitCode

-- | A single flow for a given user
data FlowView = FlowView
  { flowStart :: UTCTime,
    flowEnd :: UTCTime,
    duration :: NominalDiffTime,
    flowType :: FlowType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

sameDayThan :: Day -> (a -> UTCTime) -> a -> Bool
sameDayThan day selector a =
  utctDay (selector a) == day

senseiAPI :: Proxy SenseiAPI
senseiAPI = Proxy

data FlowType = Learning | Experimenting | Troubleshooting | Flowing | Rework | Note | Other | Meeting | End
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

instance ToHttpApiData FlowType where
  toUrlPiece f = Text.pack (show f)

instance FromHttpApiData FlowType where
  parseUrlPiece "Learning" = pure Learning
  parseUrlPiece "Experimenting" = pure Experimenting
  parseUrlPiece "Troubleshooting" = pure Troubleshooting
  parseUrlPiece "Flowing" = pure Flowing
  parseUrlPiece "Rework" = pure Rework
  parseUrlPiece "Note" = pure Note
  parseUrlPiece "End" = pure End
  parseUrlPiece "Meeting" = pure Meeting
  parseUrlPiece _txt = pure Other

data FlowState
  = FlowState
      { _flowUser :: String,
        _flowStart :: UTCTime,
        _flowDir :: String
      }
  | FlowNote
      { _flowUser :: String,
        _flowStart :: UTCTime,
        _flowDir :: String,
        _flowNote :: Text.Text
      }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Flow = Flow
  { _flowType :: FlowType,
    _flowState :: FlowState
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Grouping of `FlowView`
data Group = Day | Week | Month | Quarter | Year
  deriving (Eq, Read, Show, Ord, Generic, ToJSON, FromJSON)

instance ToHttpApiData Group where
  toUrlPiece f = Text.pack (show f)

instance FromHttpApiData Group where
  parseUrlPiece txt =
    case reads (Text.unpack txt) of
      ((g,_):_) -> pure g
      _ -> Left $ "cannot parse group " <> txt

data GroupViews
  = NoViews
  | Leaf {leafViews :: [FlowView]}
  | GroupLevel {level :: Group, groupTime :: UTCTime, subGroup :: GroupViews}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

groupViews :: [Group] -> [FlowView] -> [GroupViews]
groupViews [] views = [Leaf views]
groupViews (Day : _groups) views =
  views
    |> NE.groupBy ((==) `on` (utctDay . flowStart))
    |> mkGroupViewsBy Day
groupViews _ _ = error "unsupported group"

mkGroupViewsBy :: Group -> [NE.NonEmpty FlowView] -> [GroupViews]
mkGroupViewsBy Day =
  fmap mkGroup
  where
    mkGroup :: NE.NonEmpty FlowView -> GroupViews
    mkGroup (view :| rest) = GroupLevel Day (flowStart view) (Leaf (view : rest))
mkGroupViewsBy _ = error "unsupported group"

-- | End of work day is assumed to be 6:30pm UTC
-- TODO fix this value which is incorrect and locale dependent
endOfWorkDay :: DiffTime
endOfWorkDay = secondsToDiffTime (3600 * 16 + 1800)

-- | "pipe" operator common in other languages
-- this is basically `flip apply` which is defined as `&` in
-- the standard library
(|>) :: a -> (a -> b) -> b
(|>) = (&)
