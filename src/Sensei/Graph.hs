{-# LANGUAGE NamedFieldPuns #-}
module Sensei.Graph
  ( mkG,
    G (..),
    currentGoals,
    asGraph,
    goal,
    pop,
    shift,
    done,
    module Algebra.Graph,
  )
where

import Algebra.Graph (Graph, connect, edgeList, empty, overlay, vertex, vertexList, vertices)
import Data.Text (Text)
import Data.Tuple (swap)

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
              (_:others) -> vertices others
        Pop -> go (G full parent) ops
          where
            vs = vertexList current
            es = edgeList full
            parent = vertices $ concatMap (flip findAll es) vs
        Shift -> go (G full children) ops
          where
            vs = vertexList current
            es = edgeList full
            parents = concatMap (flip findAll es) vs
            children = vertices $ concatMap (flip findAll (map swap es)) parents

findAll :: Eq a => a -> [(a, b)] -> [b]
findAll _ [] = []
findAll a ((a', b) : as)
  | a == a' = b : findAll a as
  | otherwise = findAll a as

data G = G {unG :: Graph Text, currentG :: Graph Text}

currentGoals :: G -> [Text]
currentGoals G{currentG} = vertexList currentG

asGraph :: G -> Graph Text
asGraph = unG

data Op = Goal Text | Pop | Shift | Done

goal :: Text -> Op
goal = Goal

pop :: Op
pop = Pop

shift :: Op
shift = Shift

done :: Op
done = Done
