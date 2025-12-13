{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Sensei.Bsky.Leaflet where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Record declaring a publication
-- Lexicon: [pub.leaflet.publication](https://tangled.org/leaflet.pub/leaflet/blob/main/lexicons/pub/leaflet/publication.json)
data Publication = Publication
  { name :: Text
  -- ^ Required: Publication name (max 2000 characters)
  , base_path :: Maybe Text
  -- ^ Optional: Base path for the publication
  , description :: Maybe Text
  -- ^ Optional: Publication description (max 2000 characters)
  , icon :: Maybe Blob
  -- ^ Optional: Publication icon (image, max 1MB)
  , theme :: Maybe Theme
  -- ^ Optional: Theme configuration
  , preferences :: Maybe Preferences
  -- ^ Optional: Publication preferences
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Publication preferences
data Preferences = Preferences
  { showInDiscover :: Maybe Bool
  -- ^ Default: true
  , showComments :: Maybe Bool
  -- ^ Default: true
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Theme configuration
-- Note: Color and background image types are placeholders for now
data Theme = Theme
  { backgroundColor :: Maybe Color
  , backgroundImage :: Maybe BackgroundImage
  , primary :: Maybe Color
  , pageBackground :: Maybe Color
  , showPageBackground :: Maybe Bool
  -- ^ Default: false
  , accentBackground :: Maybe Color
  , accentText :: Maybe Color
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Placeholder for blob type (image files, max 1MB)
-- TODO: Implement proper blob handling
data Blob = Blob
  { mimeType :: Text
  , size :: Int
  , ref :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Placeholder for color type
-- TODO: Implement RGBA/RGB union type from pub.leaflet.theme.color
data Color = Color
  { r :: Int
  , g :: Int
  , b :: Int
  , a :: Maybe Double
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
