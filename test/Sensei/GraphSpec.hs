{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.GraphSpec where

import Algebra.Graph (Graph, overlay, connect, vertices, edgeList, vertexList, empty, vertex)
import Data.Maybe(mapMaybe)
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
  it "can build a simple graph" $ do
    let ops = mkG [ goal "Foo"
            , goal "Bar"
            , pop
            , goal "Baz"
            ]

        graph = asGraph ops

    edgeList graph `shouldBe` [("Bar", "Foo"), ("Baz", "Foo")]

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
            parent = vertices $ mapMaybe (flip lookup es) vs
              


data G =
  G { unG :: Graph Text, currentG :: Graph Text}
                   
asGraph :: G -> Graph Text
asGraph = unG

data Op = Goal Text | Pop

goal :: Text -> Op
goal = Goal

pop :: Op
pop = Pop
