{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.ToCSV
  ( ToCSV (..),
    ToCSVRow (..),
  )
where

import Data.Text as T
import qualified Data.Text.Lazy as L

-- | Instances of this class produce a valid CSV text output
clean :: L.Text -> L.Text
clean = L.replace "\n" "" . L.replace "," " "

class ToCSV csv where
  toCSV :: csv -> Text
  default toCSV :: Show csv => csv -> Text
  toCSV a =
    let ta = pack $ show a
     in ta `seq` ta

instance (ToCSV c) => ToCSV [c] where
  toCSV [] = empty
  toCSV (c : cs) =
    let tc = toCSV c
     in tc `seq` tc <> "\n" <> toCSV cs

instance ToCSV Text where
  toCSV = id

instance ToCSV L.Text where
  toCSV = L.toStrict . clean

instance (ToCSV a) => ToCSV (Maybe a) where
  toCSV (Just a) = toCSV a
  toCSV Nothing = empty

instance ToCSV String where
  toCSV = pack

instance ToCSV Int where
  toCSV = pack . show

instance ToCSV Bool

instance ToCSV Integer

class ToCSVRow r where
  toCSVRow :: (ToCSV a) => a -> r

instance ToCSVRow Text where
  toCSVRow = toCSV

instance (ToCSVRow r, ToCSV a) => ToCSVRow (a -> r) where
  toCSVRow a r = toCSVRow (toCSV a <> "," <> toCSV r)

instance (ToCSV a, ToCSV b) => ToCSV (a, b) where
  toCSV (a, b) = toCSV a <> "," <> toCSV b

instance (ToCSV a, ToCSV b, ToCSV c) => ToCSV (a, b, c) where
  toCSV (a, b, c) = toCSV a <> "," <> toCSV b <> "," <> toCSV c

instance (ToCSV a, ToCSV b, ToCSV c, ToCSV d) => ToCSV (a, b, c, d) where
  toCSV (a, b, c, d) = toCSV a <> "," <> toCSV b <> "," <> toCSV c <> "," <> toCSV d

instance (ToCSV a, ToCSV b, ToCSV c, ToCSV d, ToCSV e) => ToCSV (a, b, c, d, e) where
  toCSV (a, b, c, d, e) = toCSV a <> "," <> toCSV b <> "," <> toCSV c <> "," <> toCSV d <> "," <> toCSV e

instance (ToCSV a, ToCSV b, ToCSV c, ToCSV d, ToCSV e, ToCSV f) => ToCSV (a, b, c, d, e, f) where
  toCSV (a, b, c, d, e, f) = toCSV a <> "," <> toCSV b <> "," <> toCSV c <> "," <> toCSV d <> "," <> toCSV e <> "," <> toCSV f

instance (ToCSV a, ToCSV b, ToCSV c, ToCSV d, ToCSV e, ToCSV f, ToCSV g) => ToCSV (a, b, c, d, e, f, g) where
  toCSV (a, b, c, d, e, f, g) = toCSV a <> "," <> toCSV b <> "," <> toCSV c <> "," <> toCSV d <> "," <> toCSV e <> "," <> toCSV f <> "," <> toCSV g

instance (ToCSV a, ToCSV b, ToCSV c, ToCSV d, ToCSV e, ToCSV f, ToCSV g, ToCSV h) => ToCSV (a, b, c, d, e, f, g, h) where
  toCSV (a, b, c, d, e, f, g, h) = toCSV a <> "," <> toCSV b <> "," <> toCSV c <> "," <> toCSV d <> "," <> toCSV e <> "," <> toCSV f <> "," <> toCSV g <> "," <> toCSV h

instance
  ( ToCSV a,
    ToCSV b,
    ToCSV c,
    ToCSV d,
    ToCSV e,
    ToCSV f,
    ToCSV g,
    ToCSV h,
    ToCSV i
  ) =>
  ToCSV (a, b, c, d, e, f, g, h, i)
  where
  toCSV (a, b, c, d, e, f, g, h, i) =
    toCSV a <> "," <> toCSV b <> "," <> toCSV c <> "," <> toCSV d <> "," <> toCSV e <> "," <> toCSV f <> "," <> toCSV g <> "," <> toCSV h
      <> ","
      <> toCSV i
