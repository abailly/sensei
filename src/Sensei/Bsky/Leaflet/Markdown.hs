{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Sensei.Bsky.Leaflet.Markdown where

import Commonmark
  ( HasAttributes,
    IsBlock (..),
    IsInline (..),
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
import qualified Data.ByteString as BS
import Data.List (singleton)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
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
  lineBreak = [Plain "\n"]
  softBreak = [Plain "\n"]
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
  ranged _rge x = x

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

  blockQuote blocks =
    [BlockquoteBlock Blockquote {plaintext, facets}]
    where
      (facets, plaintext) = extractBlockContent blocks

  codeBlock lang plaintext =
    [CodeBlock CodeBlock' {language = Just lang, plaintext, syntaxHighlightingTheme = Nothing}]

  heading level inlines = [HeaderBlock Header {level, facets, plaintext}]
    where
      (facets, plaintext) = extractFacets inlines

  rawBlock = undefined

  referenceLinkDefinition = undefined

  list _ _spacing items =
    -- NOTE: Leaflet only supports unordered list??
    [UnorderedListBlock $ UnorderedList {children = concatMap (mapMaybe (mkListItem . adjustFacet)) items}]

adjustFacet :: BlockVariant -> BlockVariant
adjustFacet = \case
  TextBlock RichText {plaintext, facets} -> TextBlock RichText {plaintext, facets = shiftBy 2 <$> facets}
  HeaderBlock Header {level, plaintext, facets} -> HeaderBlock Header {level, plaintext, facets = shiftBy 2 <$> facets}
  b -> b
  where
    shiftBy :: Int -> Facet -> Facet
    shiftBy offset f@Facet {index = ByteSlice {byteStart, byteEnd}} =
      f {index = ByteSlice (byteStart - offset) (byteEnd - offset)}

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
          -- Use byte length, not character length, for proper UTF-8 handling
          newOffset = offset + BS.length (encodeUtf8 separator) + BS.length (encodeUtf8 blockText)
       in (newOffset, fs <> adjustedFacets, newText)

    adjustFacetOffset :: Int -> Facet -> Facet
    adjustFacetOffset offset facet@Facet {index = ByteSlice {byteStart, byteEnd}} =
      facet {index = ByteSlice {byteStart = byteStart + offset, byteEnd = byteEnd + offset}}

getBlockContent :: BlockVariant -> ([Facet], Text)
getBlockContent (TextBlock RichText {plaintext, facets}) = (facets, plaintext)
getBlockContent (HeaderBlock Header {plaintext, facets}) = (facets, plaintext)
getBlockContent (CodeBlock CodeBlock' {plaintext}) = ([], plaintext)
getBlockContent (BlockquoteBlock Blockquote {plaintext, facets}) = (facets, plaintext)
getBlockContent _ = ([], "")

extractFacets :: [Inline] -> ([Facet], Text)
extractFacets inlines =
  let Converter {facets, plaintext} = foldl (flip extractFacet) initialConverter inlines
   in (facets, plaintext)

-- | Data needed to convert `mmarkdown`'s source/col coordinates for markup into
-- `Leaflet`'s linear offset for `Facet`s.
data Converter = Converter
  { -- | Accumulated markup characters in markdown source for current line
    markup :: Int,
    -- | Text length of last line seen (in bytes)
    lastLine :: Int,
    -- | Accumulated list of facets
    facets :: [Facet],
    -- | Accumulated plain text
    plaintext :: Text,
    -- | Plaintext added on current line (for UTF-8 character-to-byte conversion)
    currentLinePlaintext :: Text
  }
  deriving (Show)

initialConverter :: Converter
initialConverter =
  Converter
    { markup = 0,
      lastLine = 0,
      facets = [],
      plaintext = "",
      currentLinePlaintext = ""
    }

extractFacet :: Inline -> Converter -> Converter
extractFacet = \case
  Decorated f rge -> makeFacet f rge
  Plain "\n" -> \Converter {facets, plaintext} ->
    Converter
      { markup = 0,
        lastLine = BS.length (encodeUtf8 plaintext) + 1,
        facets,
        plaintext = plaintext <> " ",
        currentLinePlaintext = "" -- Reset for new line
      }
  Plain t -> \Converter {plaintext, currentLinePlaintext, ..} ->
    Converter
      { plaintext = plaintext <> t,
        currentLinePlaintext = currentLinePlaintext <> t, -- Track current line text
        ..
      }
  where
    makeFacet f rge Converter {markup, lastLine, facets, plaintext, currentLinePlaintext} =
      Converter {markup = markup', lastLine, facets = facets <> [Facet {features = [f], index}], plaintext, currentLinePlaintext}
      where
        markup' =
          markup + case f of
            Code -> 2 -- `...`
            Italic -> 2 -- `*...*` or `_..._`
            Bold -> 4 -- `**...**`
            Link uri -> 4 + Text.length uri -- [...](uri)
            _ -> 0
        index = maybe (ByteSlice 0 0) toByteSlice rge
        toByteSlice (SourceRange ((beg, end) : _)) =
          -- sourceColumn gives CHARACTER positions in source (including markup)
          -- We need to convert to BYTE positions in plaintext (excluding markup)
          -- markup tracks markup characters seen so far on this line
          let -- Character position in plaintext on current line
              charPosBeg = sourceColumn beg - 1 - markup
              charPosEnd = sourceColumn end - 1 - markup'
              -- Convert character positions to byte positions using current line's plaintext
              -- Take the prefix up to the character position and measure its byte length
              prefixBeg = Text.take charPosBeg currentLinePlaintext
              prefixEnd = Text.take charPosEnd currentLinePlaintext
              bytePosInCurrentLineBeg = BS.length (encodeUtf8 prefixBeg)
              bytePosInCurrentLineEnd = BS.length (encodeUtf8 prefixEnd)
           in ByteSlice (lastLine + bytePosInCurrentLineBeg) (lastLine + bytePosInCurrentLineEnd)
        toByteSlice (SourceRange []) = ByteSlice 0 0
