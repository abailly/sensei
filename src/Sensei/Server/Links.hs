{-# LANGUAGE OverloadedStrings #-}

module Sensei.Server.Links where

import Control.Applicative ((<|>))
import Data.Text (Text, pack, unpack)
import Data.Time
import Data.Time.Lens (modL, month)
import Network.HTTP.Link as Link
import Network.URI.Extra (uriFromString)
import Sensei.Utils
import Servant

nextPageLink :: Text -> Maybe Natural -> Maybe (Link.Link URI)
nextPageLink user page = do
  p <- page <|> pure 1
  let next = show (succ p)
  uri <- uriFromString $ "/api/log/" <> unpack user <> "?page=" <> next
  pure $ Link uri [(Rel, "next"), (Link.Other "page", pack next)]

previousPageLink :: Text -> Maybe Natural -> Maybe (Link.Link URI)
previousPageLink user page = do
  p <- page
  let prev = show (pred p)
  uri <- uriFromString $ "/api/log/" <> unpack user <> "?page=" <> prev
  pure $ Link uri [(Rel, "prev"), (Link.Other "page", pack prev)]

nextDayLink :: Text -> Maybe Day -> Maybe (Link.Link URI)
nextDayLink user day = do
  d <- day
  let next = showGregorian (succ d)
  uri <- uriFromString $ "/api/flows/" <> unpack user <> "/" <> next <> "/" <> "notes"
  pure $ Link uri [(Rel, "next"), (Link.Other "page", pack next)]

previousDayLink :: Text -> Maybe Day -> Maybe (Link.Link URI)
previousDayLink user day = do
  d <- day
  let prev = showGregorian (pred d)
  uri <- uriFromString $ "/api/flows/" <> unpack user <> "/" <> prev <> "/" <> "notes"
  pure $ Link uri [(Rel, "prev"), (Link.Other "page", pack prev)]

nextMonthLink :: Text -> Maybe Day -> Maybe Day -> Maybe (Link.Link URI)
nextMonthLink user from to = do
  f <- showGregorian . modL month succ <$> from
  t <- showGregorian . modL month succ <$> to
  uri <- uriFromString $ "/api/flows/" <> unpack user <> "/summary?from=" <> f <> "&to=" <> t <> "&period=Month"
  pure $ Link uri [(Rel, "next")]

previousMonthLink :: Text -> Maybe Day -> Maybe Day -> Maybe (Link.Link URI)
previousMonthLink user from to = do
  f <- showGregorian . modL month pred <$> from
  t <- showGregorian . modL month pred <$> to
  uri <- uriFromString $ "/api/flows/" <> unpack user <> "/summary?from=" <> f <> "&to=" <> t <> "&period=Month"
  pure $ Link uri [(Rel, "prev")]
