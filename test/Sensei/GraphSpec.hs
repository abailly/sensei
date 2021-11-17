{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.GraphSpec where

import Algebra.Graph (Graph, connect, edgeList, empty, vertex)
import Control.Monad (MonadPlus (mplus))
import Data.Function ((&))
import Test.Hspec

spec :: Spec
spec = do
  it "can build a simple graph" $ do
    let ops =
          goal "Foo"
            & link (goal "Bar" <> goal "Baz")

        graph = asGraph ops

    edgeList graph `shouldBe` [("Bar", "Foo"), ("Baz", "Foo")]

asGraph :: G -> Graph String
asGraph = unG

newtype G = G {unG :: Graph String}

instance Semigroup G where
  G g <> G g' = G $ g `mplus` g'

instance Monoid G where
  mempty = G empty

link :: G -> G -> G
link (G a) (G b) = G $ a `connect` b

goal :: String -> G
goal = G . vertex
