{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Specific help functions and types to help build and manipulate
--  Sensei's types and API
module Sensei.Builder where

import Data.Functor (void)
import Sensei.API
import Sensei.TestHelper

postEvent :: Event -> WaiSession () SResponse
postEvent e =  postJSON ("/log") e

postEvent_ :: Event -> WaiSession () ()
postEvent_ =  void . postEvent

postFlow :: Flow -> WaiSession () SResponse
postFlow = postEvent . EventFlow

postFlow_ :: Flow -> WaiSession () ()
postFlow_ = postEvent_ . EventFlow

postNote_ :: NoteFlow -> WaiSession () ()
postNote_ = postEvent_ . EventNote

postTrace_ :: Trace -> WaiSession () ()
postTrace_ = postEvent_ . EventTrace
