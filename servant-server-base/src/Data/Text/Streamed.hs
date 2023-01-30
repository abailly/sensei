{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Text.Streamed
  ( toStreamWithPreamble,
    toStream,
    StreamBody,
  )
where

import Control.Monad
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Text.ToCSV

type StreamBody m a =
  (Builder -> m a) -> -- build a single chunk of stream
  m a -> -- flush stream
  m a

httpChunkedTransferMinimumSize :: Int
httpChunkedTransferMinimumSize = 3000

-- | Converts a 'Foldable' of 'ToCSV' into a streaming body of CSV rows, with
-- something else at the beginning.
toStreamWithPreamble :: (Foldable t, ToCSV csv, Monad m) => T.Text -> t csv -> StreamBody m a
toStreamWithPreamble preamble t build flush =
  let l = map toCSV (toList t)
   in foldM step ini l >>= final >> flush
  where
    -- Unscientific empirical experiment has shown that parallelizing the UTF-8 encoding is unnecessary.
    ini = (T.length preamble, E.encodeUtf8Builder preamble)
    step (!l, !c) !line =
      let !len = T.length line + l
          !c' = c <> E.encodeUtf8Builder line <> B.char7 '\n'
       in if len < httpChunkedTransferMinimumSize
            then pure (len, c')
            else build c' >> pure (0, mempty)
    final (_, !c) = build c

-- | Converts a 'Foldable' of 'ToCSV' into a streaming body of CSV rows.
toStream :: (Foldable t, ToCSV csv, Monad m) => t csv -> StreamBody m a
toStream = toStreamWithPreamble mempty
