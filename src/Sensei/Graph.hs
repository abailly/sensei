{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Sensei.Graph
  ( mkG,
    G,
    Op,
    currentGoals,
    doneGoals,
    asGraph,
    goal,
    pop,
    push,
    shift,
    done,
    module Algebra.Graph,
  )
where

import Algebra.Graph
  ( Graph,
    adjacencyList,
    connect,
    edgeList,
    empty,
    overlay,
    vertex,
    vertexList,
    vertices,
  )
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Tuple (swap)
import GHC.Generics (Generic)

mkG :: [Op] -> G
mkG = go (G empty empty empty)
  where
    go g [] = g
    go G {fullG, currentG, doneG} (op : ops) =
      case op of
        Goal v ->
          go
            G
              { fullG = ((newGoal `connect` currentG) `overlay` fullG),
                currentG = newGoal,
                doneG
              }
            ops
          where
            newGoal = vertex v
        Done -> go G {fullG, currentG = parents, doneG = newDone} ops
          where
            vs = vertexList currentG
            es = edgeList fullG
            newDone = case vs of
              (v : _) -> vertex v `overlay` doneG
              [] -> doneG
            parents = case vs of
              [v] -> vertices $ findAll v es
              [] -> empty
              (_ : others) -> vertices others
        Pop -> go G {fullG, currentG = parent, doneG} ops
          where
            vs = vertexList currentG
            es = edgeList fullG
            parent = vertices $ concatMap (`findAll` es) vs
        Push -> go G {fullG, currentG = children es vs, doneG} ops
          where
            vs = vertexList currentG
            es = edgeList fullG
        Shift -> go G {fullG, currentG = children es parents, doneG} ops
          where
            es = edgeList fullG
            parents = concatMap (flip findAll es) $ vertexList currentG

children :: [(Text, Text)] -> [Text] -> Graph Text
children es = vertices . concatMap (`findAll` (map swap es))

findAll :: Eq a => a -> [(a, b)] -> [b]
findAll _ [] = []
findAll a ((a', b) : as)
  | a == a' = b : findAll a as
  | otherwise = findAll a as

data G = G
  { fullG :: Graph Text,
    currentG :: Graph Text,
    doneG :: Graph Text
  }

currentGoals :: G -> [Text]
currentGoals G {currentG} = vertexList currentG

doneGoals :: G -> [Text]
doneGoals G {doneG} = vertexList doneG

asGraph :: G -> Graph Text
asGraph = fullG

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
