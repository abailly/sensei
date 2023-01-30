module Network.Html
  ( RawHtml,
    RawHTML,
    Rendering(..), RenderContext(..),
    mkRawHtml,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Lucid (Html, renderBS)
import Network.HTTP.Media ((//), (/:))
import Servant

data RawHTML

instance Accept RawHTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

newtype RawHtml = RawHtml LBS.ByteString

mkRawHtml :: Html a -> RawHtml
mkRawHtml = RawHtml . renderBS

instance MimeRender RawHTML RawHtml where
  mimeRender _ (RawHtml bs) = bs

instance MimeUnrender RawHTML RawHtml where
  mimeUnrender _ bs = Right (RawHtml bs)

data RenderContext = Tabular | Summary

-- | A wrapper type over some data type with more context provided as a phantom type
newtype Rendering (ctx :: RenderContext) a = Rendering a
