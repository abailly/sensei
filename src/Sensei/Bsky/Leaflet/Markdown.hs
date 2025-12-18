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
import Commonmark.Types (HasAttributes (..))
import Control.Monad.Identity (runIdentity)
import Data.Bifunctor (Bifunctor (..), bimap)
import Data.List (singleton)
import Data.Text (Text)
import qualified Data.Text as Text
import Sensei.Bsky.Leaflet
  ( Block (Block),
    BlockVariant (..),
    ByteSlice (..),
    CodeBlock' (..),
    Facet (..),
    Feature (..),
    Header (..),
    LinearDocument (..),
    RichText (..),
    UnorderedList (..), mkListItem,
  )
import Sensei.Bsky.TID (mkTid)
import Data.Maybe (mapMaybe)
import Commonmark.Extensions (mathSpec, HasMath)
import Commonmark.Extensions.Math (HasMath(..))

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
        (key, value) | not (Text.null value) ->
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
  link uri title _ = [Plain title, Decorated (Link uri) Nothing]
  image = undefined
  code txt = [Plain txt, Decorated Code Nothing]
  rawInline = undefined

instance HasMath [Inline] where
  inlineMath txt = [Plain txt, Decorated Code Nothing]  -- TODO , Decorated Math Nothing]
  displayMath txt = [Plain txt] -- TODO , Decorated DisplayMath Nothing]

instance Rangeable [BlockVariant] where
  ranged _ x = x

instance HasAttributes [BlockVariant] where
  addAttributes _ x = x

instance IsBlock [Inline] [BlockVariant] where
  paragraph inlines = [TextBlock RichText {plaintext, facets}]
    where
      (facets, plaintext) = foldr extractFacet ([], "") inlines

  plain inlines = [TextBlock RichText {plaintext, facets}]
    where
      (facets, plaintext) = foldr extractFacet ([], "") inlines
  thematicBreak = []
  blockQuote = undefined
  codeBlock lang plaintext = [CodeBlock CodeBlock' {language = Just lang, plaintext, syntaxHighlightingTheme = Nothing}]
  heading level inlines = [HeaderBlock Header {level, facets, plaintext}]
    where
      (facets, plaintext) = foldr extractFacet ([], "") inlines

  rawBlock = undefined
  referenceLinkDefinition = undefined
  list (BulletList _) _spacing items =
    [UnorderedListBlock $ UnorderedList {children = concatMap (mapMaybe mkListItem) items}]
  list _ _spacing _items = undefined

extractFacet :: Inline -> ([Facet], Text) -> ([Facet], Text)
extractFacet = \case
  Decorated f rge -> (first (Facet {features = [f], index} :))
    where
      index = (maybe (ByteSlice 0 0) toByteSlice rge)
      toByteSlice (SourceRange ((beg, end) : _)) = ByteSlice (sourceColumn beg - 1) (sourceColumn end - 1)
      toByteSlice (SourceRange []) = (ByteSlice 0 0)
  Plain t -> (second (t <>))
