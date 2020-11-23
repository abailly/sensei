module Sensei.Color where

import Data.Text(unpack, pack)
import Data.Colour.SRGB
import Data.Aeson

-- |A color for the purpose of configuring display of various graphs
newtype Color = Color { rgb :: Colour Double }
  deriving (Eq)

instance Show Color where
  show = sRGB24show . rgb

instance ToJSON Color where
  toJSON (Color color) = String $ pack $ sRGB24show color

instance FromJSON Color where
  parseJSON = withText "Color" $ \ t -> do
    let col = unpack t
    case sRGB24reads col of
      [(color, "")] -> pure $ Color color
      _ -> fail $ "cannot parse color " <> col
