module Data.Text.Decode where

import           Data.ByteString    (ByteString)
import           Data.Text
import           Data.Text.Encoding (decodeUtf8')

safeDecodeUtf8 :: ByteString -> Text
safeDecodeUtf8 = either (const mempty) Prelude.id . decodeUtf8'
