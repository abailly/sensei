module Sensei.Utils
  ( (|>),
    -- TODO: move that somewhere else...
    module Numeric.Natural,
  )
where

import Data.Function ((&))
import Numeric.Natural

-- | "pipe" operator common in other languages
-- this is basically `flip apply` which is defined as `&` in
-- the standard library
(|>) :: a -> (a -> b) -> b
(|>) = (&)
