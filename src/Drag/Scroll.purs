module Drag.Scroll where

import Prelude
import Data.Maybe (Maybe(..))

import Drag.Drag (Drag)
import Editor.Editor (Editor)
import Linear.R2 (P2)

scroll :: Editor -> P2 Number -> Editor
scroll e diff = e { view = e.view { origin = e.view.origin + diff } }

scrollDrag :: Editor -> P2 Number -> Maybe Drag
scrollDrag e start = Just { move: doScroll
                          , stop: doScroll }
  where doScroll current = e `scroll` (current - start)
