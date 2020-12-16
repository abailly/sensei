module Sensei.Utils
  ( (|>),
    sameDayThan,
    -- TODO: move that somewhere else...
    module Numeric.Natural,
  )
where

import Data.Function ((&))
import Data.Time (Day)
import Numeric.Natural

-- | "pipe" operator common in other languages
-- this is basically `flip apply` which is defined as `&` in
-- the standard library
(|>) :: a -> (a -> b) -> b
(|>) = (&)

sameDayThan :: Day -> (a -> Day) -> a -> Bool
sameDayThan day selector a =
  selector a == day
