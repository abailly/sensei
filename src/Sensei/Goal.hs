{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Sensei.Goal
  ( GoalOp (..),
    Goals(..),
    Goal(..),
    Op,
    goal,
    pop,
    push,
    shift,
    done,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text(Text)
import GHC.Generics (Generic)
import Data.String(IsString)
import Sensei.Graph
  ( Op,
    done,
    goal,
    pop,
    push,
    shift,
  )

data GoalOp = GoalOp {goalOp :: Op}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Goal = Goal Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, ToJSON, FromJSON)

-- | A representation of goals in the form of a directed graph.
data Goals = Goals { goalsGraph :: [(Goal, [Goal])] }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
