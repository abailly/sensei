{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sensei.Bsky.LeafletSpec where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..))
import Sensei.Bsky
  ( RecordWithMetadata (cid, value),
  )
import Sensei.Bsky.Leaflet (Document, publication, Publication)
import Sensei.Generators ()
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @Document)

  it "can parse simple JSON leaflet.pub documents" $ do
    document :: RecordWithMetadata Document <- either error id . eitherDecode <$> LBS.readFile "test/simple-leaflet-document.json"
    cid document `shouldBe` "bafyreidy55clvis5byc2h7o2tlqzhtevwau64kbi2465mrfbsy4ltgn72q"

  it "can parse simple JSON leaflet.pub documents" $ do
    document :: RecordWithMetadata Document <- either error id . eitherDecode <$> LBS.readFile "test/sample-leaflet-document.json"
    publication (value document) `shouldBe` Just "at://did:plc:f5bi3qiwfdxnlbvb44oudmrv/pub.leaflet.publication/3lwgwusi37s25"

  it "can parse simple JSON leaflet.pub publication" $ do
    pub :: RecordWithMetadata Publication <- either error id . eitherDecode <$> LBS.readFile "test/simple-publication.json"
    cid pub `shouldBe` "bafyreihwj6donpqcnffzswr3zblrm6rvp5li4guaotzsqmdehf3cu5tmaa"
