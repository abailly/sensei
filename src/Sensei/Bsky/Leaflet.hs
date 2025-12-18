{-# LANGUAGE TupleSections #-}

module Sensei.Bsky.Leaflet where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Sensei.Bsky.Core (BskyType (BskyType), Key, Lexicon)
import Sensei.Bsky.TID (TID, mkTid)

type PublicationLexicon = "pub.leaflet.publication"

type instance Lexicon Publication = PublicationLexicon

type instance Key Publication = TID

-- | Record declaring a publication
-- Lexicon: [pub.leaflet.publication](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/publication.json)
data Publication = Publication
  { -- | Required: Publication name (max 2000 characters)
    name :: Text,
    -- | Optional: Base path for the publication
    base_path :: Maybe Text,
    -- | Optional: Publication description (max 2000 characters)
    description :: Maybe Text,
    -- | Optional: Publication icon (image, max 1MB)
    icon :: Maybe Blob,
    -- | Optional: Theme configuration
    theme :: Maybe Theme,
    -- | Optional: Publication preferences
    preferences :: Maybe Preferences
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Publication preferences
data Preferences = Preferences
  { -- | Default: true
    showInDiscover :: Maybe Bool,
    -- | Default: true
    showComments :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Theme configuration
-- Note: Color and background image types are placeholders for now
data Theme = Theme
  { backgroundColor :: Maybe Color,
    backgroundImage :: Maybe BackgroundImage,
    primary :: Maybe Color,
    pageBackground :: Maybe Color,
    -- | Default: false
    showPageBackground :: Maybe Bool,
    accentBackground :: Maybe Color,
    accentText :: Maybe Color
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Placeholder for blob type (image files, max 1MB)
-- TODO: Implement proper blob handling
data Blob = Blob
  { mimeType :: Text,
    size :: Int,
    ref :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Placeholder for color type
-- TODO: Implement RGBA/RGB union type from pub.leaflet.theme.color
data Color = Color
  { r :: Int,
    g :: Int,
    b :: Int,
    a :: Maybe Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Placeholder for background image type
-- TODO: Implement from pub.leaflet.theme.backgroundImage
data BackgroundImage = BackgroundImage
  { image :: Blob
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type instance Lexicon Document = "pub.leaflet.document"

type instance Key Document = TID

-- | Record containing a document
-- Lexicon: [pub.leaflet.document](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/document.json)
data Document = Document
  { -- | Required: Document title (max 1280 chars, 128 graphemes)
    title :: Text,
    -- | Optional: Document description (max 3000 chars, 300 graphemes)
    description :: Maybe Text,
    -- | Required: Author (at-identifier format)
    author :: Text,
    -- | Required: Pages (union of linearDocument or canvas)
    pages :: [Page],
    -- | Optional: Tags (max 50 chars each)
    tags :: Maybe [Text],
    -- | Optional: Publication datetime
    publishedAt :: Maybe Text,
    -- | Optional: Reference to associated post (com.atproto.repo.strongRef)
    postRef :: Maybe StrongRef,
    -- | Optional: Publication (at-uri format)
    publication :: Maybe Text,
    -- | Optional: Theme configuration (pub.leaflet.publication#theme)
    theme :: Maybe Theme
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Document where
  toJSON (Document {title, description, author, pages, tags, publishedAt, postRef, publication, theme}) =
    object $
      [ "$type" .= BskyType @(Lexicon Document),
        "title" .= title,
        "author" .= author,
        "pages" .= pages
      ]
        <> optionalField "description" description
        <> optionalField "tags" tags
        <> optionalField "publishedAt" publishedAt
        <> optionalField "postRef" postRef
        <> optionalField "publication" publication
        <> optionalField "theme" theme

instance FromJSON Document where
  parseJSON = withObject "Document" $ \v -> do
    _ <- v .: "$type" :: Parser Text
    Document
      <$> v .: "title"
      <*> v .:? "description"
      <*> v .: "author"
      <*> v .: "pages"
      <*> v .:? "tags"
      <*> v .:? "publishedAt"
      <*> v .:? "postRef"
      <*> v .:? "publication"
      <*> v .:? "theme"

-- Helper function to create optional fields
optionalField :: (ToJSON a) => Text -> Maybe a -> [(KeyMap.Key, Value)]
optionalField _ Nothing = []
optionalField key (Just val) = [fromString (unpack key) .= val]

-- | Page type - union of linearDocument and canvas
-- Lexicon: [pub.leaflet.pages.linearDocument](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/pages/linearDocument.json)
data Page
  = Linear LinearDocument
  | -- | Placeholder for pub.leaflet.pages.canvas
    Canvas
  deriving stock (Eq, Show, Generic)

instance ToJSON Page where
  toJSON (Linear doc) = toJSON doc
  toJSON Canvas = object ["$type" .= ("pub.leaflet.pages.canvas" :: Text)]

instance FromJSON Page where
  parseJSON = withObject "Page" $ \v -> do
    typ <- v .: "$type" :: Parser Text
    case typ of
      "pub.leaflet.pages.linearDocument" -> Linear <$> parseJSON (Object v)
      "pub.leaflet.pages.canvas" -> pure Canvas
      _ -> fail $ "Unknown page type: " ++ show typ

type instance Lexicon LinearDocument = "pub.leaflet.pages.linearDocument"

data LinearDocument = LinearDocument
  { -- | Optional: Document ID
    -- NOTE: Should be UUIDv7 but there's no Haskell implementation widely available yet
    id :: Maybe TID,
    -- | Required: Array of content blocks
    blocks :: [Block]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LinearDocument where
  toJSON (LinearDocument {id = i, blocks = bs}) =
    object $
      [ "$type" .= ("pub.leaflet.pages.linearDocument" :: Text),
        "blocks" .= bs
      ]
        <> optionalField "id" i

instance FromJSON LinearDocument where
  parseJSON = withObject "LinearDocument" $ \v -> do
    _ <- v .: "$type" :: Parser Text
    LinearDocument
      <$> v .:? "id"
      <*> v .: "blocks"

mkSimpleDocument :: Text -> IO LinearDocument
mkSimpleDocument text = do
  tid <- mkTid
  -- TODO: parse markdown
  pure $
    LinearDocument
      { id = Just tid,
        blocks =
          [ Block
              { alignment = Nothing,
                block = TextBlock RichText {plaintext = text, facets = []}
              }
          ]
      }

type instance Lexicon Block = "pub.leaflet.pages.linearDocument#block"

-- | Block in a linear document
data Block = Block
  { -- | Required: The block content (union of various block types)
    block :: BlockVariant,
    -- | Optional: Text alignment for the block
    alignment :: Maybe TextAlignment
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Block where
  toJSON (Block {block = b, alignment = a}) =
    object $
      [ "$type" .= ("pub.leaflet.pages.linearDocument#block" :: Text),
        "block" .= b
      ]
        <> optionalField "alignment" a

instance FromJSON Block where
  parseJSON = withObject "Block" $ \v -> do
    _ <- v .: "$type" :: Parser Text
    Block
      <$> v .: "block"
      <*> v .:? "alignment"

-- | Text alignment options
data TextAlignment
  = TextAlignLeft
  | TextAlignCenter
  | TextAlignRight
  | TextAlignJustify
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Union type for different block variants
-- TODO: Implement proper types for each block variant
data BlockVariant
  = IframeBlock Iframe -- pub.leaflet.blocks.iframe
  | TextBlock RichText -- pub.leaflet.blocks.text
  | BlockquoteBlock Blockquote -- pub.leaflet.blocks.blockquote
  | HeaderBlock Header -- pub.leaflet.blocks.header
  | ImageBlock Image -- pub.leaflet.blocks.image
  | UnorderedListBlock UnorderedList -- pub.leaflet.blocks.unorderedList
  | WebsiteBlock Website -- pub.leaflet.blocks.website
  | MathBlock Math -- pub.leaflet.blocks.math
  | CodeBlock CodeBlock' -- pub.leaflet.blocks.code
  | HorizontalRuleBlock HorizontalRule -- pub.leaflet.blocks.horizontalRule
  | BskyPostBlock BskyPost' -- pub.leaflet.blocks.bskyPost
  | PageBlock PageBlock' -- pub.leaflet.blocks.page
  | PollBlock Poll -- pub.leaflet.blocks.poll
  | ButtonBlock Button -- pub.leaflet.blocks.button
  deriving stock (Eq, Show, Generic)

instance ToJSON BlockVariant where
  toJSON (TextBlock rt) = toJSON rt
  toJSON (HeaderBlock hdr) = toJSON hdr
  toJSON (CodeBlock code) = toJSON code
  toJSON (UnorderedListBlock list) = toJSON list
  -- For now, other block types are not implemented
  toJSON _ = object ["$type" .= ("pub.leaflet.blocks.unknown" :: Text)]

instance FromJSON BlockVariant where
  parseJSON = withObject "BlockVariant" $ \v -> do
    typ <- v .: "$type" :: Parser Text
    case typ of
      "pub.leaflet.blocks.text" -> TextBlock <$> parseJSON (Object v)
      "pub.leaflet.blocks.image" -> pure $ ImageBlock Image -- TODO
      "pub.leaflet.blocks.header" -> HeaderBlock <$> parseJSON (Object v)
      "pub.leaflet.blocks.code" -> CodeBlock <$> parseJSON (Object v)
      "pub.leaflet.blocks.unorderedList" -> UnorderedListBlock <$> parseJSON (Object v)
      -- For now, only text blocks are supported
      _ -> fail $ "Unsupported block type: " ++ show typ

-- Lexicon instances for block variants
type instance Lexicon Iframe = "pub.leaflet.blocks.iframe"

type instance Lexicon RichText = "pub.leaflet.blocks.text"

type instance Lexicon Blockquote = "pub.leaflet.blocks.blockquote"

type instance Lexicon Header = "pub.leaflet.blocks.header"

type instance Lexicon Image = "pub.leaflet.blocks.image"

type instance Lexicon UnorderedList = "pub.leaflet.blocks.unorderedList"

type instance Lexicon Website = "pub.leaflet.blocks.website"

type instance Lexicon Math = "pub.leaflet.blocks.math"

type instance Lexicon CodeBlock' = "pub.leaflet.blocks.code"

type instance Lexicon HorizontalRule = "pub.leaflet.blocks.horizontalRule"

type instance Lexicon BskyPost' = "pub.leaflet.blocks.bskyPost"

type instance Lexicon PageBlock' = "pub.leaflet.blocks.page"

type instance Lexicon Poll = "pub.leaflet.blocks.poll"

type instance Lexicon Button = "pub.leaflet.blocks.button"

-- Block type definitions
-- TODO: Implement proper fields for each block type based on the Leaflet lexicons

-- | Iframe block
-- Lexicon: [pub.leaflet.blocks.iframe](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/iframe.json)
data Iframe = Iframe
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Rich text block with optional formatting
-- Lexicon: [pub.leaflet.blocks.text](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/text.json)
data RichText = RichText
  { -- | Required: Plain text content
    plaintext :: Text,
    -- | Array of formatting facets
    facets :: [Facet]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RichText where
  toJSON (RichText {plaintext = pt, facets = fs}) =
    object $
      [ "$type" .= BskyType @(Lexicon RichText),
        "plaintext" .= pt,
        "facets" .= fs
      ]

instance FromJSON RichText where
  parseJSON = withObject "RichText" $ \v -> do
    _ <- v .: "$type" :: Parser Text
    RichText
      <$> v .: "plaintext"
      <*> (v .:? "facets" <&> fromMaybe [])

-- | Blockquote block
-- Lexicon: [pub.leaflet.blocks.blockquote](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/blockquote.json)
data Blockquote = Blockquote
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Header block
-- Lexicon: [pub.leaflet.blocks.header](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/header.json)
data Header = Header {plaintext :: Text, level :: Int, facets :: [Facet]}
  deriving stock (Eq, Show, Generic)

instance ToJSON Header where
  toJSON Header {plaintext, level, facets} =
    object
      [ "$type" .= BskyType @(Lexicon Header),
        "plaintext" .= plaintext,
        "level" .= level,
        "facets" .= facets
      ]

instance FromJSON Header where
  parseJSON = withObject "Header" $ \v -> do
    Header
      <$> v .: "plaintext"
      <*> v .: "level"
      <*> v .: "facets"

-- | Image block
-- Lexicon: [pub.leaflet.blocks.image](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/image.json)
data Image = Image
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Unordered list block
-- Lexicon: [pub.leaflet.blocks.unorderedList](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/unorderedList.json)
data UnorderedList = UnorderedList {children :: [ListItem]}
  deriving stock (Eq, Show, Generic)

instance ToJSON UnorderedList where
  toJSON UnorderedList {children} =
    object
      [ "$type" .= BskyType @(Lexicon UnorderedList),
        "children" .= children
      ]

instance FromJSON UnorderedList where
  parseJSON = withObject "UnorderedList" $ \v -> do
    UnorderedList
      <$> v .: "children"

data ListItem
  = TextListItem RichText [ListItem]
  | ImageListItem Image [ListItem]
  | HeaderListItem Header [ListItem]
  deriving stock (Eq, Show, Generic)

mkListItem :: BlockVariant -> Maybe ListItem
mkListItem = \case
  TextBlock rt -> Just $ TextListItem rt []
  ImageBlock img -> Just $ ImageListItem img []
  HeaderBlock hdr -> Just $ HeaderListItem hdr []
  _ -> Nothing


instance ToJSON ListItem where
  toJSON (TextListItem rt children) = object ["content" .= toJSON rt, "children" .= children]
  toJSON (HeaderListItem hdr children) = object ["content" .= toJSON hdr, "children" .= children]
  toJSON (ImageListItem img children) = object ["content" .= toJSON img, "children" .= children]

instance FromJSON ListItem where
  parseJSON = withObject "ListItem" $ \v -> do
    contentVal <- v .: "content"
    children :: [ListItem] <- v .:? "children" <&> fromMaybe []
    p <- TextListItem <$> parseJSON contentVal
      <|> HeaderListItem <$> parseJSON contentVal
      <|> ImageListItem <$> parseJSON contentVal
    pure $ p children

-- | Website block
-- Lexicon: [pub.leaflet.blocks.website](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/website.json)
data Website = Website
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Math block
-- Lexicon: [pub.leaflet.blocks.math](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/math.json)
data Math = Math
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Code block
-- Lexicon: [pub.leaflet.blocks.code](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/code.json)
data CodeBlock' = CodeBlock' {plaintext :: Text, language :: Maybe Text, syntaxHighlightingTheme :: Maybe Text}
  deriving stock (Eq, Show, Generic)

instance ToJSON CodeBlock' where
  toJSON CodeBlock' {plaintext, language, syntaxHighlightingTheme} =
    object $
      [ "$type" .= BskyType @(Lexicon CodeBlock'),
        "plaintext" .= plaintext
      ]
        <> optionalField "language" language
        <> optionalField "syntaxHighlightingTheme" syntaxHighlightingTheme

instance FromJSON CodeBlock' where
  parseJSON = withObject "CodeBlock'" $ \v -> do
    CodeBlock'
      <$> v .: "plaintext"
      <*> v .:? "language"
      <*> v .:? "syntaxHighlightingTheme"

-- | Horizontal rule block
-- Lexicon: [pub.leaflet.blocks.horizontalRule](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/horizontalRule.json)
data HorizontalRule = HorizontalRule
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Bluesky post block
-- Lexicon: [pub.leaflet.blocks.bskyPost](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/bskyPost.json)
data BskyPost' = BskyPost'
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Page block
-- Lexicon: [pub.leaflet.blocks.page](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/page.json)
data PageBlock' = PageBlock'
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Poll block
-- Lexicon: [pub.leaflet.blocks.poll](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/poll.json)
data Poll = Poll
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Button block
-- Lexicon: [pub.leaflet.blocks.button](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/blocks/button.json)
data Button = Button
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Annotation of a sub-string within rich text
-- Lexicon: [pub.leaflet.richtext.facet](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/richtext/facet.json)
data Facet = Facet
  { -- | Required: Byte range where the formatting applies
    index :: ByteSlice,
    -- | Required: Array of formatting features
    features :: [Feature]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Byte range for text formatting (zero-indexed, inclusive start, exclusive end)
data ByteSlice = ByteSlice
  { -- | Required: Start index (inclusive, zero-indexed)
    byteStart :: Int,
    -- | Required: End index (exclusive, zero-indexed)
    byteEnd :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Text formatting feature types
data Feature
  = -- | URL link
    Link {uri :: Text}
  | -- | DID mention
    DidMention {did :: Text}
  | -- | AT URI mention
    AtMention {atURI :: Text}
  | -- | Inline code formatting
    Code
  | -- | Text highlighting
    Highlight
  | -- | Underline styling
    Underline
  | -- | Strikethrough styling
    Strikethrough
  | -- | Identifier for linking
    Id {featureId :: Maybe Text}
  | -- | Bold text formatting
    Bold
  | -- | Italic text formatting
    Italic
  deriving stock (Eq, Show, Generic)

instance ToJSON Feature where
  toJSON (Link u) = object ["$type" .= ("pub.leaflet.richtext.facet#link" :: Text), "uri" .= u]
  toJSON (DidMention d) = object ["$type" .= ("pub.leaflet.richtext.facet#didMention" :: Text), "did" .= d]
  toJSON (AtMention u) = object ["$type" .= ("pub.leaflet.richtext.facet#atMention" :: Text), "atURI" .= u]
  toJSON Code = object ["$type" .= ("pub.leaflet.richtext.facet#code" :: Text)]
  toJSON Highlight = object ["$type" .= ("pub.leaflet.richtext.facet#highlight" :: Text)]
  toJSON Underline = object ["$type" .= ("pub.leaflet.richtext.facet#underline" :: Text)]
  toJSON Strikethrough = object ["$type" .= ("pub.leaflet.richtext.facet#strikethrough" :: Text)]
  toJSON (Id fid) =
    object $
      ["$type" .= ("pub.leaflet.richtext.facet#id" :: Text)]
        <> optionalField "featureId" fid
  toJSON Bold = object ["$type" .= ("pub.leaflet.richtext.facet#bold" :: Text)]
  toJSON Italic = object ["$type" .= ("pub.leaflet.richtext.facet#italic" :: Text)]

instance FromJSON Feature where
  parseJSON = withObject "Feature" $ \v -> do
    typ <- v .: "$type" :: Parser Text
    case typ of
      "pub.leaflet.richtext.facet#link" -> Link <$> v .: "uri"
      "pub.leaflet.richtext.facet#didMention" -> DidMention <$> v .: "did"
      "pub.leaflet.richtext.facet#atMention" -> AtMention <$> v .: "atURI"
      "pub.leaflet.richtext.facet#code" -> pure Code
      "pub.leaflet.richtext.facet#highlight" -> pure Highlight
      "pub.leaflet.richtext.facet#underline" -> pure Underline
      "pub.leaflet.richtext.facet#strikethrough" -> pure Strikethrough
      "pub.leaflet.richtext.facet#id" -> Id <$> v .:? "featureId"
      "pub.leaflet.richtext.facet#bold" -> pure Bold
      "pub.leaflet.richtext.facet#italic" -> pure Italic
      _ -> fail $ "Unknown feature type: " ++ show typ

-- | Placeholder for strong reference type
-- TODO: Implement com.atproto.repo.strongRef
data StrongRef = StrongRef
  { uri :: Text,
    cid :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
