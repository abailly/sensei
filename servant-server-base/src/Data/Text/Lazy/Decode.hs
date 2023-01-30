module Data.Text.Lazy.Decode where

import           Data.ByteString.Lazy    (ByteString)
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding (decodeUtf8')

safeDecodeUtf8 :: ByteString -> Text
safeDecodeUtf8 = either (const mempty) Prelude.id . decodeUtf8'
