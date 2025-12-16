{-# LANGUAGE RecordWildCards #-}

module Sensei.Graph (
    mkG,
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
    add,
    link,
    module Algebra.Graph,
    remove,
) where

import Algebra.Graph (
    Context (..),
    Graph,
    adjacencyList,
    connect,
    context,
    edge,
    edgeList,
    empty,
    overlay,
    removeEdge,
    removeVertex,
    vertex,
    vertexList,
    vertices,
 )
import Data.Aeson (FromJSON, ToJSON)
import Data.List ((\\))
import Data.Text (Text)
import Data.Tuple (swap)
import GHC.Generics (Generic)

mkG :: [Op] -> G
mkG = go (G empty empty empty)
  where
    go g [] = g
    go G{fullG, currentG, doneG} (op : ops) =
        case op of
            Goal v ->
                go
                    G
                        { fullG = (newGoal `connect` currentG) `overlay` fullG
                        , currentG = newGoal
                        , doneG
                        }
                    ops
              where
                newGoal = vertex v
            Add v ->
                go
                    G
                        { fullG =
                            (currentG `connect` newGoal)
                                `overlay` (newGoal `connect` parents)
                                `overlay` updatedG
                        , currentG = newCurrent
                        , doneG
                        }
                    ops
              where
                vs = vertexList currentG
                es = edgeList fullG
                newGoal = vertex v
                (updatedG, newCurrent, parents) = case vs of
                    (d : _) ->
                        let parentVs = findAll d es
                         in ( foldr (removeEdge d) fullG parentVs
                            , currentG
                            , vertices parentVs
                            )
                    [] -> (fullG, newGoal, empty)
            Remove v ->
                go G{fullG = fullG', currentG = currentG', doneG = doneG'} ops
              where
                transitiveInputs g acc w
                    | w `elem` acc = acc
                    | otherwise =
                        case context (== w) g of
                            Just Context{inputs} -> concatMap (transitiveInputs g (inputs <> acc)) inputs
                            Nothing -> []
                parents =
                    case context (== v) fullG of
                        Just Context{outputs} -> outputs \\ removed
                        Nothing -> []
                removed = v : transitiveInputs fullG [] v
                fullG' = foldr removeVertex fullG removed
                currentG' = foldr (overlay . vertex) (foldr removeVertex currentG removed) parents
                doneG' = foldr removeVertex doneG removed
            Done -> go G{fullG, currentG = newCurrent, doneG = newDone} ops
              where
                vs = vertexList currentG
                es = edgeList fullG
                newDone = case vs of
                    (v : _) -> vertex v `overlay` doneG
                    [] -> doneG
                newCurrent = case vs of
                    [v] -> vertices $ findAll v es
                    [] -> empty
                    (_ : others) -> vertices others
            Pop -> go G{fullG, currentG = parent, doneG} ops
              where
                vs = vertexList currentG
                es = edgeList fullG
                parent = vertices $ concatMap (`findAll` es) vs
            Push -> go G{fullG, currentG = children es vs, doneG} ops
              where
                vs = vertexList currentG
                es = edgeList fullG
            Shift -> go G{fullG, currentG = children es parents, doneG} ops
              where
                es = edgeList fullG
                parents = concatMap (`findAll` es) $ vertexList currentG
            Link from to ->
                go G{fullG = (from `edge` to) `overlay` fullG, ..} ops

children :: [(Text, Text)] -> [Text] -> Graph Text
children es = vertices . concatMap (`findAll` map swap es)

findAll :: Eq a => a -> [(a, b)] -> [b]
findAll _ [] = []
findAll a ((a', b) : as)
    | a == a' = b : findAll a as
    | otherwise = findAll a as

data G = G
    { fullG :: Graph Text
    , currentG :: Graph Text
    , doneG :: Graph Text
    }
    deriving (Show)

currentGoals :: G -> [Text]
currentGoals G{currentG} = vertexList currentG

doneGoals :: G -> [Text]
doneGoals G{doneG} = vertexList doneG

asGraph :: G -> Graph Text
asGraph = fullG

data Op
    = Goal Text
    | Pop
    | Push
    | Shift
    | Done
    | Add Text
    | Remove Text
    | Link Text Text
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

add :: Text -> Op
add = Add

remove :: Text -> Op
remove = Remove

link :: Text -> Text -> Op
link = Link
