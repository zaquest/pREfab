module Main where

import Prelude
import Data.Polygon as Poly
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Editor.Editor (mkEditor)
import Editor.Render (drawWorkArea)
import Graphics.Canvas ( CANVAS, getCanvasElementById, getContext2D
                       , setCanvasWidth, setCanvasHeight )
import Grid (CGrid)
import Interactive (setUpHandlers)
import Linear.R2 (p2)
import Utils (fromJust)
import Put (PUT)
import Window as W

poly :: Poly.Poly2 CGrid
poly = fromJust $ Poly.mkPoly [p2 0 2, p2 16 0, p2 8 8, p2 (-1) 16]

main :: forall e
      . Eff ( canvas :: CANVAS
            , dom :: DOM
            , console :: CONSOLE
            , ref :: REF
            , put :: PUT
            | e)
            Unit
main = do
  {width, height} <- W.getSize
  canvas <- fromJust <$> getCanvasElementById "work-area"
  setCanvasWidth width canvas
  setCanvasHeight height canvas
  ctx <- getContext2D canvas
  let editor = mkEditor ctx width height poly
  drawWorkArea editor
  setUpHandlers editor
