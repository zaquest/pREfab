module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
--import Linear.R2 (p2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Editor (drawWorkArea, mkContext, mkEditor)
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, setCanvasWidth, setCanvasHeight)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Window (innerHeight, innerWidth) as DOM
import Data.Polygon as Poly
import Linear.R2 (p2)
import Grid (CGrid)
--import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, ready, select)
--import Data.Foreign.Class (readNumber)

--showValidity (Editor e) =
--  case e.validity of
--    Nothing -> log "Nothing"
--    Just v  -> log ("Just (" <> show v.lo <> " " <> show v.hi <> " " <> show v.validity <> ")")

poly :: Poly.Poly2 CGrid
poly = Poly.Poly $ Poly.fromFoldable [p2 0 2, p2 16 0, p2 8 8, p2 (-1) 16]

main :: forall e. Eff (canvas :: CANVAS, dom :: DOM, console :: CONSOLE | e) Unit
main = do
  window <- DOM.window
  width <- toNumber <$> DOM.innerWidth window
  height <- toNumber <$> DOM.innerHeight window
  mcanvas <- getCanvasElementById "work-area"
  case mcanvas of
    Nothing -> log "Failed to find #work-area"
    Just canvas -> do
      setCanvasWidth width canvas
      setCanvasHeight height canvas
      ctx <- mkContext <$> getContext2D canvas <*> pure width <*> pure height
      log $ "Found #work-area and set width = " <> show ctx.width <> ", height = " <> show ctx.height
      let editor = mkEditor ctx poly
      drawWorkArea editor
