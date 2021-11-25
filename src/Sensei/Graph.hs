{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Sensei.Graph
  ( mkG,
    G (..),
    Op,
    currentGoals,
    asGraph,
    goal,
    pop,
    push,
    shift,
    done,
    module Algebra.Graph,
  )
where

import Algebra.Graph (Graph,
                      connect, edgeList, adjacencyList, empty, overlay, vertex, vertexList, vertices)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Tuple (swap)
import GHC.Generics (Generic)

mkG :: [Op] -> G
mkG = go (G empty empty)
  where
    go g [] = g
    go (G full current) (op : ops) =
      case op of
        Goal v -> go (G ((newGoal `connect` current) `overlay` full) newGoal) ops
          where
            newGoal = vertex v
        Done -> go (G full parents) ops
          where
            vs = vertexList current
            es = edgeList full
            parents = case vs of
              [v] -> vertices $ findAll v es
              [] -> empty
              (_ : others) -> vertices others
        Pop -> go (G full parent) ops
          where
            vs = vertexList current
            es = edgeList full
            parent = vertices $ concatMap (`findAll` es) vs
        Push -> go (G full $ children es vs) ops
          where
            vs = vertexList current
            es = edgeList full
        Shift -> go (G full $ children es parents) ops
          where
            es = edgeList full
            parents = concatMap (flip findAll es) $ vertexList current

children :: [(Text, Text)] -> [Text] -> Graph Text
children es = vertices . concatMap (`findAll` (map swap es))

findAll :: Eq a => a -> [(a, b)] -> [b]
findAll _ [] = []
findAll a ((a', b) : as)
  | a == a' = b : findAll a as
  | otherwise = findAll a as

data G = G {unG :: Graph Text, currentG :: Graph Text}

currentGoals :: G -> [Text]
currentGoals G {currentG} = vertexList currentG

asGraph :: G -> Graph Text
asGraph = unG

data Op = Goal Text | Pop | Push | Shift | Done
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

goal :: Text -> Op
goal = Goal

pop :: Op
pop = Pop

push :: Op
push = Push

shift :: Op
shift = Shift

done :: Op
done = Done
