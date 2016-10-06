module Interactive where

import Prelude
import Data.Array (concatMap, (..))
import Data.Monoid ((<>))
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.First (First(..), runFirst)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref ( REF, Ref, newRef, readRef, modifyRef
                             , modifyRef', writeRef )
import Graphics.Canvas (CANVAS)
import Canvas as C
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery ( JQueryEvent, JQuery, select, on
                                , getValue, preventDefault )
import DOM.WebStorage (STORAGE, ForeignStorage)
import DOM (DOM)
import Window as W
import JQuery (clientX, clientY)
import Data.Foreign (readString)
import Data.Either (Either(..))
import Data.Bifunctor (lmap)
import Data.Int (fromString)

import Linear.R2 (p2, P2, r2)
import Editor.Editor (Editor)
import Editor.Render (drawWorkArea)
import Editor.WorkArea (mkWorkArea)
import Drag.Drag (Drag)
import Drag.Scroll (scrollDrag)
import Drag.EditPoint (editPointDrag)

import Put (PUT, newOutput, runPut, Output)
import Serialize (put)
import RedEclipse.Octree (Octree(Leaf))
import RedEclipse.Prefab ( Prefab(..), PrefabFile(..)
                         , PrefabHeader(..), PrefabGeom(..)
                         , defaultPrefabFileHeader, mapzIVec3 )
import RedEclipse.Face (toPlane)
import History (History)
import History as H
import Storage (saveHistory)
import Data.Polygon (defaultPoly, Poly2)
import Grid (CGrid)

type State = { drag :: Maybe Drag
             , editor :: Editor
             , storage :: ForeignStorage
             , history :: History (Poly2 CGrid) }

mouseXY :: forall e. JQueryEvent -> Eff (dom :: DOM | e) (P2 Number)
mouseXY e = p2 <$> clientX e <*> clientY e

onMouseDown :: forall e
             . Ref State
            -> JQueryEvent
            -> JQuery
            -> Eff ( dom     :: DOM
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
            -> Eff ( canvas  :: CANVAS
                   , dom     :: DOM
                   , ref     :: REF
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
          -> Eff ( canvas  :: CANVAS
                 , dom     :: DOM
                 , ref     :: REF
                 , storage :: STORAGE
                 | e ) Unit
onMouseUp stateRef event jq = do
  current <- mouseXY event
  s <- readRef stateRef
  case s.drag of
    Nothing -> pure unit
    Just drag -> do
      let e = drag.stop current
      let h = if H.present s.history == e.workArea.poly
                then s.history
                else H.push s.history e.workArea.poly
      writeRef stateRef (s { editor = e, history = h, drag = Nothing })
      drawWorkArea e
      saveHistory s.storage h

onZoom :: forall e
        . Boolean
       -> Ref State
       -> JQueryEvent
       -> JQuery
       -> Eff ( canvas  :: CANVAS
              , dom     :: DOM
              , ref     :: REF
              | e ) Unit
onZoom zoomIn stateRef event jq = do
  preventDefault event
  editor <- modifyRef' stateRef \s ->
    let z = s.editor.view.zoom + diff in
    let zs = if 0.2 <= z
                then s { editor = s.editor { view = s.editor.view { zoom = z } } }
                else s
     in { state: zs, value: zs.editor }
  drawWorkArea editor
  where diff = if zoomIn then 0.1 else -0.1

foreign import saveAsFile :: forall e. String -> Output -> Eff ( dom :: DOM | e ) Unit
foreign import jsAlert :: forall e. String -> Eff ( dom :: DOM | e ) Unit

onSave :: forall e
        . Ref State
       -> JQueryEvent
       -> JQuery
       -> Eff ( canvas  :: CANVAS
              , console :: CONSOLE
              , dom     :: DOM
              , put     :: PUT
              , ref     :: REF
              | e ) Unit
onSave stateRef event jq = do
  preventDefault event
  jqDepth <- select "#depth input"
  eStrVal <- readString <$> getValue jqDepth
  let eVal = do str <- lmap show eStrVal
                maybe (Left "Not Int") Right (fromString str)
  case eVal of
    Left err -> log err
    Right depth -> do
      -- convert poly to prefab geom
      state <- readRef stateRef
      let poly = state.editor.workArea.poly
      case toPlane poly of
        Nothing ->  jsAlert "Invalid polygon"
        Just plane -> do
          let geom = PrefabGeom $ map Leaf (concatMap (\ys -> concatMap (\z -> map (\_ -> z) (1..depth)) ys) plane.plane)

          -- produce prefab file
          let phdr = PrefabHeader { orients: mapzIVec3 0 0 0 -- not important
                                  , sizes: mapzIVec3 (r2 plane.size).x depth (r2 plane.size).y
                                  , grid: 8
                                  , orient: 1 }
          let prefab = Prefab { header: phdr, geom: geom }
          let pfile = PrefabFile { header: defaultPrefabFileHeader
                                 , prefab: prefab }
          output <- newOutput
          runPut (put pfile) output
          saveAsFile "prefab.obr" output

onResize :: forall e
          . Ref State
         -> Eff ( canvas  :: CANVAS
                , dom     :: DOM
                , ref     :: REF
                | e ) Unit
onResize stateRef = do
  s <- readRef stateRef
  let canvas = C.contextCanvas s.editor.context
  size@{width, height} <- W.getSize
  C.setSize canvas size
  let s' = s { editor = s.editor { view = s.editor.view { width = width, height = height } } }
  writeRef stateRef s'
  drawWorkArea s'.editor

-- TODO: merge undo/redo here
onUndo :: forall e
        . Ref State
       -> Eff ( canvas :: CANVAS
              , dom     :: DOM
              , ref     :: REF
              , storage :: STORAGE
              | e ) Unit
onUndo stateRef = do
  result <- modifyRef' stateRef \s ->
    let hp = H.undo s.history
     in case hp.value of
          Nothing -> { state: s, value: Nothing }
          Just poly ->
            let e = s.editor { workArea = mkWorkArea poly }
                s' = s { editor = e, history = hp.state }
             in { state: s', value: Just s' }
  case result of
    Nothing -> pure unit
    Just state -> do
      drawWorkArea state.editor
      saveHistory state.storage state.history

onRedo :: forall e
        . Ref State
       -> Eff ( canvas :: CANVAS
              , dom     :: DOM
              , ref     :: REF
              , storage :: STORAGE
              | e ) Unit
onRedo stateRef = do
  result <- modifyRef' stateRef \s ->
    let hp = H.redo s.history
     in case hp.value of
          Nothing -> { state: s, value: Nothing }
          Just poly ->
            let e = s.editor { workArea = mkWorkArea poly }
                s' = s { editor = e, history = hp.state }
             in { state: s', value: Just s' }
  case result of
    Nothing -> pure unit
    Just state -> do
      drawWorkArea state.editor
      saveHistory state.storage state.history

onReset :: forall e
         . Ref State
        -> Eff ( canvas :: CANVAS
               , dom     :: DOM
               , ref     :: REF
               , storage :: STORAGE
               | e ) Unit
onReset stateRef = do
  result <- modifyRef' stateRef \s ->
    let e = s.editor { workArea = mkWorkArea defaultPoly }
        h = H.push s.history defaultPoly
        s' = s { editor = e, history = h }
     in { state: s', value: s' }
  drawWorkArea result.editor
  saveHistory result.storage result.history

setUpHandlers :: forall e
               . Editor
              -> ForeignStorage
              -> History (Poly2 CGrid)
              -> Eff ( canvas  :: CANVAS
                     , console :: CONSOLE
                     , dom     :: DOM
                     , put     :: PUT
                     , ref     :: REF
                     , storage :: STORAGE
                     | e ) Unit
setUpHandlers editor storage history = do
  stateRef <- newRef { drag: Nothing
                     , editor: editor
                     , storage: storage
                     , history: history }
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
  W.onResize (\_ -> onResize stateRef)
  jqUndo <- select "#undo"
  on "click" (\_ _ -> onUndo stateRef) jqUndo
  jqRedo <- select "#redo"
  on "click" (\_ _ -> onRedo stateRef) jqRedo
  jqReset <- select "#reset"
  on "click" (\_ _ -> onReset stateRef) jqReset
