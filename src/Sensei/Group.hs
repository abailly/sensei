{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
module Sensei.Group where

import Data.Aeson
    ( FromJSON, ToJSON )
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Data.Time
    ( LocalTime(..),
      TimeOfDay )
import GHC.Generics ( Generic )
import Servant
    ( FromHttpApiData(parseUrlPiece), ToHttpApiData(toUrlPiece) )
import Sensei.FlowView
    ( FlowView(flowStart), normalizeViewsForDay )
import Sensei.Utils ( (|>) )

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
