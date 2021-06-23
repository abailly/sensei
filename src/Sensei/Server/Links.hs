{-# LANGUAGE OverloadedStrings #-}

module Sensei.Server.Links (nextPageLink, previousPageLink, nextDayLink, previousDayLink, periodLinks, module Link) where

import Control.Applicative ((<|>))
import Data.Text (Text, pack, unpack)
import Data.Time
import Network.HTTP.Link as Link
import Network.URI.Extra (uriFromString)
import Sensei.Group (Group, toPeriod)
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

periodLinks :: Text -> Day -> Day -> Group -> Maybe [Link.Link URI]
periodLinks userName fromDay toDay period = do
  nextHeader <- periodLink userName fromDay toDay period succ "next"
  prevHeader <- periodLink userName fromDay toDay period pred "prev"
  pure [prevHeader, nextHeader]

periodLink :: Text -> Day -> Day -> Group -> (Int -> Int) -> Text -> Maybe (Link.Link URI)
periodLink user from to period increment tag = do
  let rollOver = toPeriod period
      f = showGregorian . rollOver increment $ from
      t = showGregorian . rollOver increment $ to
  uri <- uriFromString $ "/api/flows/" <> unpack user <> "/summary?from=" <> f <> "&to=" <> t <> "&period=" <> show period
  pure $ Link uri [(Rel, tag), (Link.Other "from", pack f), (Link.Other "to", pack t), (Link.Other "period", pack (show period))]
