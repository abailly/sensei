module Sensei.Graph
(mkG, G(..), asGraph,
 goal, pop, shift,
 module Algebra.Graph)
  where

import Algebra.Graph (Graph, overlay, connect, vertices, edgeList, vertexList, empty, vertex)
import Data.Tuple(swap)
import Data.Text (Text)

mkG :: [Op ] -> G
mkG = go (G empty empty)
  where
    go g [] = g
    go (G full current) (op:ops) = 
      case op of
        Goal v -> go (G ((newGoal `connect` current) `overlay` full) newGoal) ops
          where newGoal = vertex v
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
              
findAll :: Eq a => a -> [(a,b)] -> [b]
findAll _ [] = []
findAll a ((a',b): as)
 | a == a' = b : findAll a as
 | otherwise = findAll a as

data G =
  G { unG :: Graph Text, currentG :: Graph Text}
                   
asGraph :: G -> Graph Text
asGraph = unG

data Op = Goal Text | Pop | Shift

goal :: Text -> Op
goal = Goal

pop :: Op
pop = Pop

shift :: Op
shift = Shift
