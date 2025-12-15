{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Sensei.Bsky.Leaflet where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Sensei.Bsky.Core (Key, Lexicon)
import Sensei.Bsky.TID (TID)

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
  deriving anyclass (ToJSON, FromJSON)

-- | Page type - union of linearDocument and canvas
-- Lexicon: [pub.leaflet.pages.linearDocument](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/pages/linearDocument.json)
data Page
  = LinearDocument
      { -- | Optional: Document ID
        id :: Maybe Text,
        -- | Required: Array of content blocks
        blocks :: [Block]
      }
  | -- | Placeholder for pub.leaflet.pages.canvas
    Canvas
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Block in a linear document
data Block = Block
  { -- | Required: The block content (union of various block types)
    block :: BlockVariant,
    -- | Optional: Text alignment for the block
    alignment :: Maybe TextAlignment
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  = IframeBlock -- pub.leaflet.blocks.iframe
  | TextBlock Text -- pub.leaflet.blocks.text (simplified for now)
  | BlockquoteBlock -- pub.leaflet.blocks.blockquote
  | HeaderBlock -- pub.leaflet.blocks.header
  | ImageBlock -- pub.leaflet.blocks.image
  | UnorderedListBlock -- pub.leaflet.blocks.unorderedList
  | WebsiteBlock -- pub.leaflet.blocks.website
  | MathBlock -- pub.leaflet.blocks.math
  | CodeBlock -- pub.leaflet.blocks.code
  | HorizontalRuleBlock -- pub.leaflet.blocks.horizontalRule
  | BskyPostBlock -- pub.leaflet.blocks.bskyPost
  | PageBlock -- pub.leaflet.blocks.page
  | PollBlock -- pub.leaflet.blocks.poll
  | ButtonBlock -- pub.leaflet.blocks.button
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Placeholder for strong reference type
-- TODO: Implement com.atproto.repo.strongRef
data StrongRef = StrongRef
  { uri :: Text,
    cid :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
