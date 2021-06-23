module Sensei.Color where

import Data.Aeson
import qualified Data.Colour.Names as Colour
import Data.Colour.SRGB
import Data.String
import Data.Text (pack, unpack)
import System.Random

-- | A color for the purpose of configuring display of various graphs
newtype Color = Color {rgb :: Colour Double}
  deriving (Eq)

instance IsString Color where
  fromString s =
    case sRGB24reads s of
      [(color, "")] -> Color color
      _ -> error $ "cannot parse color " <> s

instance Show Color where
  show = sRGB24show . rgb

instance ToJSON Color where
  toJSON (Color color) = String $ pack $ sRGB24show color

instance FromJSON Color where
  parseJSON = withText "Color" $ \t -> do
    let col = unpack t
    case sRGB24reads col of
      [(color, "")] -> pure $ Color color
      _ -> fail $ "cannot parse color " <> col

instance Random Color where
  randomR = undefined

  random s =
    let (r, s') = random s
        (g, s'') = random s'
        (b, s''') = random s''
     in (Color $ sRGB24 r g b, s''')

randomColors ::
  Int -> [Color]
randomColors seed =
  randoms (mkStdGen seed)

white :: Color
white = Color $ Colour.white
