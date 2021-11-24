{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.GraphSpec where

import Sensei.Graph
import Test.Hspec

spec :: Spec
spec = do
  it "can build a simple graph" $ do
    let ops =
          mkG
            [ goal "Foo",
              goal "Bar",
              pop,
              goal "Baz",
              shift,
              goal "Quux"
            ]

        graph = asGraph ops

    edgeList graph `shouldBe` [("Bar", "Foo"), ("Baz", "Foo"), ("Quux", "Bar"), ("Quux", "Baz")]

  it "retrieve current goal(s)" $ do
    let ops =
          mkG
            [ goal "Foo",
              goal "Bar",
              pop,
              goal "Baz",
              shift,
              goal "Quux"
            ]

    currentGoals ops `shouldBe` ["Quux"]

  it "change current goals when 'done'" $ do
    let ops =
          mkG
            [ goal "Foo",
              goal "Bar",
              pop,
              goal "Baz",
              shift,
              goal "Quux",
              done
            ]

    currentGoals ops `shouldBe` ["Bar", "Baz"]

  it "remove one goal from goals when 'done'" $ do
    let ops =
          mkG
            [ goal "Foo",
              goal "Bar",
              pop,
              goal "Baz",
              shift,
              goal "Quux",
              done,
              done
            ]

    currentGoals ops `shouldBe` ["Baz"]

  it "remove set children as current on 'push'" $ do
    let ops =
          mkG
            [ goal "Foo",
              goal "Bar",
              pop,
              goal "Baz",
              pop,
              push
            ]

    currentGoals ops `shouldBe` ["Bar", "Baz"]
