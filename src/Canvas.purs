module Canvas
     ( setSize
     , contextCanvas
     ) where

import Prelude
import Control.Monad.Eff (Eff)
import Graphics.Canvas ( CANVAS, setCanvasWidth, setCanvasHeight
                       , CanvasElement, Context2D )

setSize :: forall e
               . CanvasElement
              -> {width :: Number, height :: Number}
              -> Eff ( canvas :: CANVAS | e ) Unit
setSize canvas {width, height} = void do
  setCanvasWidth width canvas
  setCanvasHeight height canvas

foreign import contextCanvas :: Context2D -> CanvasElement
