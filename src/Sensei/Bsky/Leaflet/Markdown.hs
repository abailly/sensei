{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sensei.Bsky.Leaflet.Markdown where

import Commonmark
  ( HasAttributes,
    IsBlock (..),
    IsInline (..),
    ListType (..),
    Rangeable (..),
    SourceRange (..),
    defaultSyntaxSpec,
    parseCommonmarkWith,
    sourceColumn,
    tokenize,
  )
import Commonmark.Extensions (HasMath, mathSpec)
import Commonmark.Extensions.Math (HasMath (..))
import Commonmark.Types (HasAttributes (..))
import Control.Monad.Identity (runIdentity)
import Data.Bifunctor (Bifunctor (..), bimap)
import Data.List (singleton)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Sensei.Bsky.Leaflet
  ( Block (Block),
    BlockVariant (..),
    Blockquote (..),
    ByteSlice (..),
    CodeBlock' (..),
    Facet (..),
    Feature (..),
    Header (..),
    LinearDocument (..),
    RichText (..),
    UnorderedList (..),
    mkListItem,
  )
import Sensei.Bsky.TID (mkTid)

-- | Extract YAML frontmatter metadata from markdown text.
-- Metadata is delimited by "---" at the start and end, with key-value pairs in between.
-- Returns the metadata as a list of (key, value) pairs and the remaining markdown text.
extractMetadata :: Text -> ([(Text, Text)], Text)
extractMetadata text
  | Text.isPrefixOf "---" text =
      let textLines = Text.lines text
       in case textLines of
            (_firstDelim : rest) ->
              let (metadataLines, afterMetadata) = break (== "---") rest
               in case afterMetadata of
                    (_secondDelim : remaining) ->
                      let metadata = parseMetadataLines metadataLines
                          remainingText = Text.unlines remaining
                       in (metadata, remainingText)
                    _ -> ([], text) -- No closing delimiter, treat as regular text
            _ -> ([], text)
  | otherwise = ([], text) -- No frontmatter
  where
    parseMetadataLines :: [Text] -> [(Text, Text)]
    parseMetadataLines = mapMaybe parseLine

    parseLine :: Text -> Maybe (Text, Text)
    parseLine line =
      case Text.breakOn ":" line of
        (key, value)
          | not (Text.null value) ->
              Just (Text.strip key, Text.strip (Text.drop 1 value))
        _ -> Nothing

-- | Create a LinearDocument from markdown text
mkMarkdownDocument :: Text -> IO (Either String LinearDocument)
mkMarkdownDocument text = do
  tid <- mkTid
  case parseMarkdown text of
    Left err -> pure $ Left err
    Right markdown ->
      pure $
        Right $
          LinearDocument
            { id = Just tid,
              blocks = markdown
            }

parseMarkdown :: Text -> Either String [Block]
parseMarkdown txt =
  bimap show (fmap (`Block` Nothing)) $
    runIdentity $
      parseCommonmarkWith (mathSpec <> defaultSyntaxSpec) (tokenize "" txt)

-- FIXME: Need to create a data type that encompasses both Inline and
-- Blocks, then provide a mapping to leaflet's blocks and facets. This
-- data type should be quite similat to `Html a` from the commonmark
-- library, basically a tree structure along with Monoid and
-- commonmark specific instances to handle IsInline and IsBlock
-- typeclasses instances.
data Inline = Plain Text | Decorated Feature (Maybe SourceRange)
  deriving (Eq, Show)

instance Rangeable [Inline] where
  ranged range = fmap (ranged range)

instance Rangeable Inline where
  ranged range = \case
    Plain t -> Plain t
    Decorated f _ -> Decorated f (Just range)

instance HasAttributes [Inline] where
  addAttributes _ x = x

instance IsInline [Inline] where
  lineBreak = []
  softBreak = []
  str = singleton . Plain
  entity = undefined
  escapedChar = undefined
  emph inl = inl <> [Decorated Italic Nothing]
  strong inl = inl <> [Decorated Bold Nothing]
  link uri title inl = inl <> [Plain title, Decorated (Link uri) Nothing]
  image = undefined
  code txt = [Plain txt, Decorated Code Nothing]
  rawInline = undefined

instance HasMath [Inline] where
  inlineMath txt = [Plain txt, Decorated Code Nothing] -- TODO , Decorated Math Nothing]
  displayMath txt = [Plain txt] -- TODO , Decorated DisplayMath Nothing]

instance Rangeable [BlockVariant] where
  ranged _ x = x

instance HasAttributes [BlockVariant] where
  addAttributes _ x = x

instance IsBlock [Inline] [BlockVariant] where
  paragraph inlines = [TextBlock RichText {plaintext, facets}]
    where
      (facets, plaintext) = extractFacets inlines

  plain inlines = [TextBlock RichText {plaintext, facets}]
    where
      (facets, plaintext) = extractFacets inlines
  thematicBreak = []
  blockQuote blocks = [BlockquoteBlock Blockquote {plaintext, facets}]
    where
      (facets, plaintext) = extractBlockContent blocks
  codeBlock lang plaintext = [CodeBlock CodeBlock' {language = Just lang, plaintext, syntaxHighlightingTheme = Nothing}]
  heading level inlines = [HeaderBlock Header {level, facets, plaintext}]
    where
      (facets, plaintext) = extractFacets inlines

  rawBlock = undefined
  referenceLinkDefinition = undefined
  list (BulletList _) _spacing items =
    [UnorderedListBlock $ UnorderedList {children = concatMap (mapMaybe mkListItem) items}]
  list _ _spacing _items = undefined

-- | Extract plaintext and facets from a list of blocks (for blockquotes)
-- Concatenates all text from nested blocks with newlines and adjusts facet positions
extractBlockContent :: [BlockVariant] -> ([Facet], Text)
extractBlockContent blocks =
  let (_, facets, plaintext) = foldl extractFromBlock (0, [], "") blocks
   in (facets, plaintext)
  where
    extractFromBlock :: (Int, [Facet], Text) -> BlockVariant -> (Int, [Facet], Text)
    extractFromBlock (offset, fs, txt) block =
      let (blockFacets, blockText) = getBlockContent block
          adjustedFacets = map (adjustFacetOffset offset) blockFacets
          separator = if Text.null txt then "" else "\n"
          newText = txt <> separator <> blockText
          newOffset = offset + Text.length separator + Text.length blockText
       in (newOffset, fs <> adjustedFacets, newText)

    getBlockContent :: BlockVariant -> ([Facet], Text)
    getBlockContent (TextBlock RichText {plaintext, facets}) = (facets, plaintext)
    getBlockContent (HeaderBlock Header {plaintext, facets}) = (facets, plaintext)
    getBlockContent (CodeBlock CodeBlock' {plaintext}) = ([], plaintext)
    getBlockContent (BlockquoteBlock Blockquote {plaintext, facets}) = (facets, plaintext)
    getBlockContent _ = ([], "")

    adjustFacetOffset :: Int -> Facet -> Facet
    adjustFacetOffset offset facet@Facet {index = ByteSlice {byteStart, byteEnd}} =
      facet {index = ByteSlice {byteStart = byteStart + offset, byteEnd = byteEnd + offset}}

extractFacets :: [Inline] -> ([Facet], Text)
extractFacets inlines =
  let (_, f, t) = foldl (flip extractFacet) (0, [], "") inlines
   in (f, t)

extractFacet :: Inline -> (Int, [Facet], Text) -> (Int, [Facet], Text)
extractFacet = \case
  Decorated f rge -> makeFacet f rge
  Plain t -> \(offset, f, t') -> (offset, f, t' <> t)
  where
    makeFacet f rge (offset, fs, t) =
      (offset + offset', fs <> [Facet {features = [f], index}], t)
      where
        offset' = case f of
          Code -> 2
          Italic -> 2
          Bold -> 2
          Link uri -> 4 + Text.length uri
          _ -> 0
        index = maybe (ByteSlice 0 0) toByteSlice rge
        toByteSlice (SourceRange ((beg, end) : _)) =
          -- NOTE: range is a line/column, not sure if 1 or zero based for columns
          -- let's asssume it's 1-based. The range includes delimitation characters so
          -- we should deduce those.
          -- But we must also deduce all previously accumulated offsets in the same block
          -- of text, corresponding to previous facet/feature. Those must be accumulated
          -- as we traverse the list of inlines
          ByteSlice (sourceColumn beg - 1 - offset) (sourceColumn end - 1 - (offset + offset'))
        toByteSlice (SourceRange []) = ByteSlice 0 0
