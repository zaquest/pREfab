module Interactive where

import Prelude
import Data.Array (concatMap, (..))
import Data.Monoid ((<>))
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.First (First(..), runFirst)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref ( REF, Ref, newRef, readRef, modifyRef
                             , modifyRef' )
import Graphics.Canvas (CANVAS)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery ( JQueryEvent, JQuery, select, on
                                , getValue, preventDefault )
import DOM (DOM)
import JQuery (clientX, clientY)
import Data.Foreign (readString)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Bifunctor (lmap)

import Linear.R2 (p2, P2, r2)
import Editor.Editor (Editor)
import Editor.Render (drawWorkArea)
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

type State = { drag :: Maybe Drag
             , editor :: Editor }

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
  editor <- modifyRef' stateRef \s ->
    let ds = case s.drag of
               Nothing -> s
               Just drag -> { drag: Nothing
                            , editor: drag.stop current }
    in { state: ds, value: ds.editor }
  drawWorkArea editor

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
       -> Eff ( console :: CONSOLE
              , dom     :: DOM
              , ref     :: REF
              , canvas  :: CANVAS
              , put     :: PUT
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

setUpHandlers :: forall e
               . Editor
              -> Eff ( console :: CONSOLE
                     , ref     :: REF
                     , dom     :: DOM
                     , canvas  :: CANVAS
                     , put     :: PUT
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
