{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.GraphSpec where

import Algebra.Graph (Graph, overlay, connect, edgeList, empty, vertex)
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
  it "can build a simple graph" $ do
    let ops = mkG [ goal "Foo"
            , goal "Bar"
            , goal "Baz"
            ]

        graph = asGraph ops

    edgeList graph `shouldBe` [("Bar", "Foo"), ("Baz", "Bar")]

mkG :: [Op ] -> G
mkG = go (G empty empty)
  where
    go g [] = g
    go (G full current) (op:ops) = 
      case op of
        Goal v -> go (G ((vertex v `connect` current) `overlay` full) (vertex v)) ops
        Pop -> undefined

data G =
  G { unG :: Graph Text, currentG :: Graph Text }
                   
asGraph :: G -> Graph Text
asGraph = unG

data Op = Goal Text | Pop

goal :: Text -> Op
goal = Goal

pop :: Op
pop = Pop
