{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Specific help functions and types to help build and manipulate
--  Sensei's types and API
module Sensei.Builder where

import Data.Functor (void)
import Data.Text.Encoding (encodeUtf8)
import Sensei.API
import Sensei.TestHelper
import Servant

postEvent_ :: Event -> WaiSession () ()
postEvent_ (EventTrace t) = postTrace_ t
postEvent_ (EventFlow f) = postFlow_ f
postEvent_ (EventNote n) = postNote_ n

postFlow :: Flow -> WaiSession () SResponse
postFlow f@Flow {_flowType} =
  postJSON ("/flows/arnaud/" <> encodeUtf8 (toUrlPiece _flowType)) (EventFlow f)

postFlow_ :: Flow -> WaiSession () ()
postFlow_ = void . postFlow

postNote_ :: NoteFlow -> WaiSession () ()
postNote_ n =
  postJSON_ ("/flows/arnaud/Note") (EventNote n)

postTrace_ :: Trace -> WaiSession () ()
postTrace_ trace =
  postJSON_ "/trace" (EventTrace trace)
