{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.GraphSpec where

import Algebra.Graph (Graph, connect, edgeList, empty, overlay, vertex)
import Control.Monad (MonadPlus (mplus))
import Data.Function ((&))
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
  it "can build a simple graph" $ do
    let ops =
          mempty
            & goal "Foo"
            & link (goal' "Bar" <> goal' "Baz")

        graph = asGraph ops

    edgeList graph `shouldBe` [("Bar", "Foo"), ("Baz", "Foo")]

asGraph :: G -> Graph Text
asGraph = unG

newtype G = G {unG :: Graph Text}

instance Semigroup G where
  G g <> G g' = G $ g `mplus` g'

instance Monoid G where
  mempty = G empty

link :: G -> G -> G
link (G a) (G b) = G $ a `connect` b

goal :: Text -> G -> G
goal v (G g) = G $ g `overlay` vertex v

goal' :: Text -> G
goal' = G . vertex
