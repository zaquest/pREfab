module Drag.Drag where

import Linear.R2 (P2)
import Editor.Editor (Editor)

type Drag = { move :: P2 Number -> Editor
            , stop :: P2 Number -> Editor }
