module Main where

import Prelude
import Data.Polygon as Poly
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Window (innerHeight, innerWidth) as DOM
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Editor.Editor (mkEditor)
import Editor.Render (drawWorkArea)
import Graphics.Canvas ( CANVAS, getCanvasElementById, getContext2D
                       , setCanvasWidth, setCanvasHeight )
import Grid (CGrid)
import Interactive (setUpHandlers)
import Linear.R2 (p2)
import Utils (fromJust)

--showValidity (Editor e) =
--  case e.validity of
--    Nothing -> log "Nothing"
--    Just v  -> log ("Just (" <> show v.lo <> " " <> show v.hi <> " " <> show v.validity <> ")")

poly :: Poly.Poly2 CGrid
poly = fromJust $ Poly.mkPoly [p2 0 2, p2 16 0, p2 8 8, p2 (-1) 16]

main :: forall e
      . Eff ( canvas :: CANVAS
            , dom :: DOM
            , console :: CONSOLE
            , ref :: REF
            | e)
            Unit
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
      ctx <- getContext2D canvas
      log $ "Found #work-area and set width = " <> show width <> ", height = " <> show height
      let editor = mkEditor ctx width height poly
      drawWorkArea editor
      setUpHandlers editor
