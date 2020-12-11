{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Specific help functions and types to help build and manipulate
--  Sensei's types and API
module Sensei.Builder where

import Data.Text.Encoding (encodeUtf8)
import Sensei.API
import Sensei.TestHelper
import Servant

postFlow_ :: Flow -> WaiSession () ()
postFlow_ Flow {_flowType, _flowState} =
  postJSON_ ("/flows/arnaud/" <> encodeUtf8 (toUrlPiece _flowType)) _flowState

postTrace_ :: Trace -> WaiSession () ()
postTrace_ trace =
  postJSON_ "/trace" trace
