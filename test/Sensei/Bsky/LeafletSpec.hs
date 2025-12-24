{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sensei.Bsky.LeafletSpec where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Sensei.Bsky (Block (..), BlockVariant (..), Blockquote (..), ByteSlice (..), Facet (..), Feature (..), LinearDocument (LinearDocument, blocks), ListItem (..), RecordWithMetadata (cid, value), RichText (..), UnorderedList (..))
import Sensei.Bsky.Leaflet (Document, Publication, publication)
import Sensei.Bsky.Leaflet.Markdown (extractMetadata, mkMarkdownDocument)
import Sensei.Generators ()
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec (Spec, describe, it, pendingWith, shouldBe, shouldSatisfy)

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

  describe "Markdown to Leaflet conversion" $ do
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

    it "correctly assign facet for inline code annotations" $ do
      let markdown = "Un entier est ici construit à l'aide de la méthode `succ` et de la constante `Zero`:"
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = TextBlock RichText {plaintext, facets}} -> do
              plaintext `shouldBe` "Un entier est ici construit à l'aide de la méthode succ et de la constante Zero:"
              length facets `shouldBe` 2
              facets
                `shouldBe` [ Facet {index = ByteSlice 51 55, features = [Code]},
                             Facet {index = ByteSlice 75 79, features = [Code]}
                           ]
            other -> error $ "Expected a single rich text block, got: " <> show other
        other -> error $ "Expected a single text block, got: " <> show other

    it "computes facets offsets as bytes not characters" $ do
      let markdown = "Un entier est ici construit à l'aide de la méthode `succ` et de la constante `Zero`:"
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = TextBlock RichText {plaintext, facets}} -> do
              plaintext `shouldBe` "Un entier est ici construit à l'aide de la méthode succ et de la constante Zero:"
              length facets `shouldBe` 2
              facets
                `shouldBe` [ Facet {index = ByteSlice 51 55, features = [Code]},
                             Facet {index = ByteSlice 75 79, features = [Code]}
                           ]
            other -> error $ "Expected a single rich text block, got: " <> show other
        other -> error $ "Expected a single text block, got: " <> show other

    it "correctly assign facet for inline emphasis annotations" $ do
      let markdown = "In the beginning, there was Dungeons & Dragons, the ancestor of all modern *Role Playing Games*."
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = TextBlock RichText {plaintext, facets}} -> do
              plaintext `shouldBe` "In the beginning, there was Dungeons & Dragons, the ancestor of all modern Role Playing Games."
              facets
                `shouldBe` [Facet {index = ByteSlice 75 93, features = [Italic]}]
            other -> error $ "Expected a single rich text block, got: " <> show other
        other -> error $ "Expected a single text block, got: " <> show other

    it "correctly assign facet for inline formatting annotations on multiline paragraphs" $ do
      let markdown = "In the beginning, there was *Dungeons & Dragons*,\nthe ancestor of all modern **Role Playing Games**.\nIt was fun `and` exciting."
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = TextBlock RichText {plaintext, facets}} -> do
              plaintext `shouldBe` "In the beginning, there was Dungeons & Dragons, the ancestor of all modern Role Playing Games. It was fun and exciting."
              facets
                `shouldBe` [ Facet {index = ByteSlice 28 46, features = [Italic]},
                             Facet {index = ByteSlice 75 93, features = [Bold]},
                             Facet {index = ByteSlice 106 109, features = [Code]}
                           ]
            other -> error $ "Expected a single rich text block, got: " <> show other
        other -> error $ "Expected a single text block, got: " <> show other

    it "correctly assign facet for link annotation" $ do
      let markdown = "This post was triggered by a [tweet from Alberto Brandolini](https://twitter.com/ziobrando/status/737619202538758145) on  [The rise and fall of the Dungeon Master](https://medium.com/@ziobrando/the-rise-and-fall-of-the-dungeon-master-c2d511eed12f#.erkso3y88)"
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = TextBlock RichText {plaintext, facets}} -> do
              plaintext `shouldBe` "This post was triggered by a tweet from Alberto Brandolini on  The rise and fall of the Dungeon Master"
              facets
                `shouldBe` [ Facet
                               { index = ByteSlice 29 58,
                                 features = [Link "https://twitter.com/ziobrando/status/737619202538758145"]
                               },
                             Facet
                               { index = ByteSlice 63 102,
                                 features = [Link "https://medium.com/@ziobrando/the-rise-and-fall-of-the-dungeon-master-c2d511eed12f#.erkso3y88"]
                               }
                           ]
            other -> error $ "Expected a single rich text block, got: " <> show other
        other -> error $ "Expected a single text block, got: " <> show other

    it "FIXME: assign code facet for inline math" $ do
      let markdown = "In the beginning, there was $\\sin(x)$, the ancestor of functions."
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = TextBlock RichText {plaintext, facets}} -> do
              plaintext `shouldBe` "In the beginning, there was \\sin(x), the ancestor of functions."
              facets
                `shouldBe` [Facet {index = ByteSlice 28 35, features = [Code]}]
            other -> error $ "Expected a single rich text block, got: " <> show other
        other -> error $ "Expected a single text block, got: " <> show other

    it "correctly assign multiple facets" $ do
      pendingWith "FIXME: correctly handle nested facets"
      let markdown = "This post was triggered by a [*tweet* from `Alberto Brandolini`](https://twitter.com/ziobrando/status/737619202538758145)"
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = TextBlock RichText {plaintext, facets}} -> do
              plaintext `shouldBe` "This post was triggered by a tweet from Alberto Brandolini"
              facets
                `shouldBe` [ Facet
                               { index = ByteSlice 29 34,
                                 features = [Italic]
                               },
                             Facet
                               { index = ByteSlice 40 58,
                                 features = [Code]
                               },
                             Facet
                               { index = ByteSlice 29 58,
                                 features = [Link "https://twitter.com/ziobrando/status/737619202538758145"]
                               }
                           ]
            other -> error $ "Expected a single rich text block, got: " <> show other
        other -> error $ "Expected a single text block, got: " <> show other

    it "converts simple blockquote to Blockquote block" $ do
      let markdown = "> This is a simple blockquote."
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = BlockquoteBlock Blockquote {plaintext, facets}} -> do
              plaintext `shouldBe` "This is a simple blockquote."
              facets `shouldBe` []
            other -> error $ "Expected a blockquote block, got: " <> show other
        other -> error $ "Expected a single block, got: " <> show other

    it "converts blockquote with inline formatting" $ do
      let markdown = "> This is a blockquote with *emphasis* and **bold** text."
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = BlockquoteBlock Blockquote {plaintext, facets}} -> do
              plaintext `shouldBe` "This is a blockquote with emphasis and bold text."
              length facets `shouldBe` 2
              facets
                `shouldBe` [ Facet {index = ByteSlice 28 36, features = [Italic]},
                             Facet {index = ByteSlice 41 45, features = [Bold]}
                           ]
            other -> error $ "Expected a blockquote block, got: " <> show other
        other -> error $ "Expected a single block, got: " <> show other

    it "converts blockquote with inline code" $ do
      let markdown = "> Use the `println` function to print output."
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = BlockquoteBlock Blockquote {plaintext, facets}} -> do
              plaintext `shouldBe` "Use the println function to print output."
              facets
                `shouldBe` [Facet {index = ByteSlice 10 17, features = [Code]}]
            other -> error $ "Expected a blockquote block, got: " <> show other
        other -> error $ "Expected a single block, got: " <> show other

    it "converts multi-paragraph blockquote" $ do
      let markdown =
            Text.unlines
              [ "> First paragraph of the quote.",
                ">",
                "> Second paragraph of the quote."
              ]
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = BlockquoteBlock Blockquote {plaintext, facets}} -> do
              plaintext `shouldBe` "First paragraph of the quote.\nSecond paragraph of the quote."
              facets `shouldBe` []
            other -> error $ "Expected a blockquote block, got: " <> show other
        other -> error $ "Expected a single block, got: " <> show other

    it "converts multi-paragraph blockquote with inline formatting" $ do
      let markdown =
            Text.unlines
              [ "> First line of first paragraph of the quote with *emphasis*.",
                "> Second line with **bold** of the first paragraph of the quote.",
                ">",
                "> Second paragraph of the quote."
              ]
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block {block = BlockquoteBlock Blockquote {plaintext, facets}} -> do
              plaintext `shouldBe` "First line of first paragraph of the quote with emphasis. Second line with bold of the first paragraph of the quote.\nSecond paragraph of the quote."
              facets
                `shouldBe` [ Facet {index = ByteSlice {byteStart = 50, byteEnd = 58}, features = [Italic]},
                             Facet {index = ByteSlice {byteStart = 77, byteEnd = 81}, features = [Bold]}
                           ]
            other -> error $ "Expected a blockquote block, got: " <> show other
        other -> error $ "Expected a single block, got: " <> show other

    it "converts unordered list block with inline markup" $ do
      let markdown =
            Text.unlines
              [ "* First line of the list with *emphasis* word",
                "* Second line with `Code` fragment",
                "* Third **bold** line with"
              ]
      result <- mkMarkdownDocument markdown
      case result of
        Right LinearDocument {blocks = [firstBlock]} -> do
          case firstBlock of
            Block
              { block =
                  UnorderedListBlock
                    UnorderedList
                      { children =
                          [ TextListItem RichText {facets} [],
                            TextListItem _two [],
                            TextListItem _three []
                            ]
                      }
              } -> do
                facets `shouldBe` [Facet {index = ByteSlice {byteStart = 28, byteEnd = 36}, features = [Italic]}]
            other -> error $ "Expected a blockquote block, got: " <> show other
        other -> error $ "Expected a single block, got: " <> show other
