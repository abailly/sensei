module Data.Text.Lazy.FromText where

import           Data.Text.Lazy as LT

class FromText a where
  fromText :: LT.Text -> a
