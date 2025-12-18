{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sensei.Bsky.LeafletSpec where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..))
import qualified Data.Text.IO as Text
import Sensei.Bsky
  ( RecordWithMetadata (cid, value),
  )
import Sensei.Bsky.Leaflet (Document, publication, Publication, blocks)
import Sensei.Bsky.Leaflet.Markdown (mkMarkdownDocument, extractMetadata)
import Sensei.Generators ()
import qualified Data.Text as Text
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (Spec, it, shouldBe, shouldSatisfy)

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

  it "converts markdown to leaflet document" $ do
    markdown <- Text.readFile "test/sample-markdown.md"
    result <- mkMarkdownDocument markdown
    result `shouldSatisfy` \case
      Right doc -> not (null (blocks doc))
      Left _ -> False

  it "extracts YAML frontmatter metadata from markdown" $ do
    let markdown =
          Text.unlines
            [ "---",
              "title: Test Article",
              "author: John Doe",
              "date: 2024-01-15",
              "---",
              "",
              "# Heading",
              "",
              "Content here."
            ]
        (metadata, remaining) = extractMetadata markdown

    metadata `shouldBe` [("title", "Test Article"), ("author", "John Doe"), ("date", "2024-01-15")]
    Text.strip remaining `shouldBe` Text.strip (Text.unlines ["# Heading", "", "Content here."])

  it "handles markdown without frontmatter" $ do
    let markdown = Text.unlines ["# Heading", "", "Content here."]
        (metadata, remaining) = extractMetadata markdown

    metadata `shouldBe` []
    remaining `shouldBe` markdown

  it "handles markdown with malformed frontmatter" $ do
    let markdown = Text.unlines ["---", "title: Test", "Content without closing delimiter"]
        (metadata, remaining) = extractMetadata markdown

    metadata `shouldBe` []
    remaining `shouldBe` markdown
