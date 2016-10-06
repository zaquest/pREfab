module Main where

import Prelude
import Data.Polygon (defaultPoly)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.WebStorage (STORAGE, getLocalStorage)
import Editor.Editor (mkEditor)
import Editor.Render (drawWorkArea)
import Graphics.Canvas ( CANVAS, getCanvasElementById, getContext2D
                       , setCanvasWidth, setCanvasHeight )
import Interactive (setUpHandlers)
import Utils (fromJust)
import Put (PUT)
import Window as W
import History as H
import Data.Maybe (fromMaybe)
import Storage (loadHistory)

main :: forall e
      . Eff ( canvas :: CANVAS
            , dom :: DOM
            , console :: CONSOLE
            , ref :: REF
            , put :: PUT
            , storage :: STORAGE
            | e)
            Unit
main = do
  storage <- getLocalStorage
  mhistory <- loadHistory storage
  let history = fromMaybe (H.start defaultPoly 20 30) mhistory
  {width, height} <- W.getSize
  canvas <- fromJust <$> getCanvasElementById "work-area"
  setCanvasWidth width canvas
  setCanvasHeight height canvas
  ctx <- getContext2D canvas
  let editor = mkEditor ctx width height (H.present history)
  drawWorkArea editor
  setUpHandlers editor storage history
