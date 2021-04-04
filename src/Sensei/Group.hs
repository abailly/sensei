{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
      TimeOfDay, toGregorian )
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
  | Leaf a
  | GroupLevel {level :: Group, groupTime :: LocalTime, subGroup :: [GroupViews a]}
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Functor)

class Groupable a where
  groupTimestamp :: a -> LocalTime
  normalizeForDay :: LocalTime -> LocalTime -> [a] -> [a]
  
instance Groupable FlowView where
  groupTimestamp = flowStart
  normalizeForDay = normalizeViewsForDay

type Month = (Integer, Int)

localMonth :: LocalTime -> Month
localMonth = (\(y,m,_) -> (y,m)) . toGregorian . localDay

groupViews :: forall a . Groupable a => TimeOfDay -> TimeOfDay -> [Group] -> [a] -> [GroupViews a]
groupViews _ _ [] views = fmap Leaf views
groupViews startOfDay endOfDay (Month : groups) views =
  views
    |> NE.groupBy ((==) `on` (localMonth . groupTimestamp))
    |> fmap monthlyGroup
  where
    monthlyGroup :: NE.NonEmpty a -> GroupViews a
    monthlyGroup subs@(g :| _) = GroupLevel Month monthTimestamp $ groupViews startOfDay endOfDay groups (NE.toList subs)
      where
        monthTimestamp = groupTimestamp g 
groupViews startOfDay endOfDay (Day : _groups) views =
  views
    |> NE.groupBy ((==) `on` (localDay . groupTimestamp))
    |> mkDailyGroupViewsBy startOfDay endOfDay
groupViews _ _ _ _ = error "unsupported group"

mkDailyGroupViewsBy :: forall a . Groupable a => TimeOfDay -> TimeOfDay -> [NE.NonEmpty a] -> [GroupViews a]
mkDailyGroupViewsBy startOfDay endOfDay =
  fmap mkGroup
  where
    mkGroup :: NE.NonEmpty a -> GroupViews a
    mkGroup (view :| rest) = GroupLevel Day (groupTimestamp view) (fmap Leaf (normalized (view :| rest)))
    
    normalized :: NE.NonEmpty a -> [a]
    normalized (view :| rest) =
      let viewDay = localDay (groupTimestamp view)
       in normalizeForDay (LocalTime viewDay startOfDay) (LocalTime viewDay endOfDay) (view : rest)

