module Interactive where

import Prelude
import Data.Monoid ((<>))
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.First (First(..), runFirst)
import Control.Monad.Eff (Eff)
import Graphics.Canvas (CANVAS)
import Control.Monad.Eff.Console (CONSOLE, logShow{-, log-})
import Control.Monad.Eff.JQuery (JQueryEvent, JQuery, select, on, getValue, preventDefault)
import DOM (DOM)
import JQuery (clientX, clientY)
import Data.Foreign (readString)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Bifunctor (lmap)

import Ref (REF, Ref, newRef, readRef, {-writeRef,-} modifyRef)
import Linear.R2 (p2, P2)
import Editor.Editor (Editor)
import Editor.Render (drawWorkArea)
import Drag.Drag (Drag)
import Drag.Scroll (scrollDrag)
import Drag.EditPoint (editPointDrag)

type State = { drag :: Maybe Drag
             , editor :: Editor }

type PointDrag = Poly2 CGrid -> Either (P2 CGrid) (Edge2 CGrid) -> P2 CGrid -> Poly2 CGrid

mouseXY :: forall e. JQueryEvent -> Eff (dom :: DOM | e) (P2 Number)
mouseXY e = p2 <$> clientX e <*> clientY e

onMouseDown :: forall e
             . Ref State
            -> JQueryEvent
            -> JQuery
            -> Eff ( console :: CONSOLE
                   , dom     :: DOM
                   , ref     :: REF
                   | e ) Unit
onMouseDown stateRef event jq = void do
  start <- mouseXY event
  modifyRef stateRef \s ->
    s { drag = runFirst ((First (editPointDrag s.editor start))
                        <> (First (scrollDrag s.editor start))) }

onMouseMove :: forall e
             . Ref State
            -> JQueryEvent
            -> JQuery
            -> Eff ( console :: CONSOLE
                   , dom     :: DOM
                   , ref     :: REF
                   , canvas  :: CANVAS
                   | e ) Unit
onMouseMove stateRef event jq = do
  state <- readRef stateRef
  current <- mouseXY event
  case state.drag of
    Nothing -> pure unit
    Just drag -> drawWorkArea (drag.move current)

onMouseUp :: forall e
           . Ref State
          -> JQueryEvent
          -> JQuery
          -> Eff ( console :: CONSOLE
                 , dom     :: DOM
                 , ref     :: REF
                 , canvas  :: CANVAS
                 | e ) Unit
onMouseUp stateRef event jq = do
  current <- mouseXY event
  s' <- modifyRef stateRef \s ->
    case s.drag of
      Nothing -> s
      Just drag -> { drag: Nothing
                   , editor: drag.stop current }
  drawWorkArea s'.editor

onZoom :: forall e
        . Boolean
       -> Ref State
       -> JQueryEvent
       -> JQuery
       -> Eff ( console :: CONSOLE
              , dom     :: DOM
              , ref     :: REF
              , canvas  :: CANVAS
              | e ) Unit
onZoom zoomIn stateRef event jq = do
  preventDefault event
  s' <- modifyRef stateRef \s ->
    let z = s.editor.view.zoom + diff
    in if 0.2 <= z
        then s { editor = s.editor { view = s.editor.view { zoom = z } } }
        else s
  drawWorkArea s'.editor
  where diff = if zoomIn then 0.1 else -0.1

onSave :: forall e
        . Ref State
       -> JQueryEvent
       -> JQuery
       -> Eff ( console :: CONSOLE
              , dom     :: DOM
              , ref     :: REF
              , canvas  :: CANVAS
              | e ) Unit
onSave stateRef event jq = do
  preventDefault event
  jqDepth <- select "#depth input"
  eStrVal <- readString <$> getValue jqDepth
  let eVal = do str <- lmap show eStrVal
                maybe (Left "Not Int") Right (fromString str)
  logShow (eVal :: Either String Int)

setUpHandlers :: forall e
               . Editor
              -> Eff ( console :: CONSOLE
                     , ref     :: REF
                     , dom     :: DOM
                     , canvas  :: CANVAS
                     | e ) Unit
setUpHandlers editor = do
  stateRef <- newRef { drag: Nothing, editor: editor }
  jq <- select "#work-area"
  on "mousedown" (onMouseDown stateRef) jq
  on "mousemove" (onMouseMove stateRef) jq
  on "mouseup" (onMouseUp stateRef) jq
  jqZoomIn <- select "#zoom-in"
  on "click" (onZoom true stateRef) jqZoomIn
  jqZoomOut <- select "#zoom-out"
  on "click" (onZoom false stateRef) jqZoomOut
  jqSave <- select "#save"
  on "click" (onSave stateRef) jqSave
