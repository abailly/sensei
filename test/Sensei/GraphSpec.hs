{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.GraphSpec where

import Sensei.Graph
import Test.Hspec

spec :: Spec
spec = do
  it "can build a simple graph" $ do
    let ops = mkG [ goal "Foo"
            , goal "Bar"
            , pop
            , goal "Baz"
            , shift
            , goal "Quux"
            ]

        graph = asGraph ops

    edgeList graph `shouldBe` [("Bar", "Foo"), ("Baz", "Foo"), ("Quux", "Bar"), ("Quux", "Baz")]

