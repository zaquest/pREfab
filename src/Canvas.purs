module Canvas
     ( setSize
     , contextCanvas
     ) where

import Prelude
import Control.Monad.Eff (Eff)
import Graphics.Canvas ( CANVAS, setCanvasWidth, setCanvasHeight
                       , CanvasElement, Context2D )

-- | `setSize canvasEl {width: w, height h}` sets size of a canvas,
-- | specified by `canvasEl`, to have width `w` and height `h`.
setSize :: forall e
               . CanvasElement
              -> {width :: Number, height :: Number}
              -> Eff ( canvas :: CANVAS | e ) Unit
setSize canvas {width, height} = void do
  setCanvasWidth width canvas
  setCanvasHeight height canvas

-- | `contextCanvas ctx` retrieves canvas element to which drawing
-- | context `ctx` is bound.
foreign import contextCanvas :: Context2D -> CanvasElement
