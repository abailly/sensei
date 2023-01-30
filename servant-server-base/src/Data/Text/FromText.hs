module Data.Text.FromText where

import           Data.Text as T

class FromText a where
  fromText :: T.Text -> a
