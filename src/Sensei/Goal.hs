{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Sensei.Goal
  ( GoalOp (..),
    Op,
    goal,
    pop,
    push,
    shift,
    done,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
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
