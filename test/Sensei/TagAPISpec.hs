{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.TagAPISpec where

import Sensei.TestHelper
import Test.Hspec

spec :: Spec
spec = withApp app $ do
  it "POST /api/tag/<event id> with tags text register tags for given event" $ \_ -> do
    pending
