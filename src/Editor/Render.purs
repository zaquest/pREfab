module Editor.Render where

import Prelude
import Control.Monad.Eff (Eff)
import Math (pi)
import Data.Int (toNumber, round)
import Data.Traversable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, beginPath, setStrokeStyle, setLineWidth, stroke, moveTo, lineTo, setFillStyle, arc, fill, fillRect, clearRect)

import Linear.R2 (P2(..), p2rec, (^*), (^/))
import Data.Polygon (Poly2, Polygon(..), uncons)
import RedEclipse.Face (Plane)
import Editor.Style (FillStyle, PointStyle, LineStyle)
import Editor.Editor (Editor)
import Editor.View (View, fromGrid, subGridSize, mainGridSize)
import Editor.WorkArea (EditPoint)
import Utils (enumerate, whileM_)

drawGrid :: forall e. LineStyle -> View -> Number -> Context2D -> Eff (canvas :: CANVAS | e) Unit
drawGrid style { origin, width, height } gridSz ctx = void do
  beginPath ctx
  setStrokeStyle style.style ctx
  setLineWidth style.width ctx
  whileM_ start.x (\x -> x <= width) $ \x -> do
    moveTo ctx x 0.0
    lineTo ctx x height
    pure (x + gridSz)
  whileM_ start.y (\y -> y <= height) $ \y -> do
    moveTo ctx 0.0 y
    lineTo ctx width y
    pure (y + gridSz)
  stroke ctx
  where start = p2rec $ origin - ((toNumber <<< round) <$> (origin ^/ gridSz)) ^* gridSz

drawPoint :: forall e. PointStyle -> P2 Number -> Context2D -> Eff (canvas :: CANVAS | e) Unit
drawPoint style (P2 p) ctx = void do
  beginPath ctx
  setFillStyle style.style ctx
  arc ctx thePoint
  fill ctx
  where thePoint = { x: p.x, y: p.y, r: style.radius, start: 0.0, end: 2.0 * pi }

drawPoly :: forall e. LineStyle -> PointStyle -> Poly2 Number -> Context2D -> Eff (canvas :: CANVAS | e) Unit
drawPoly lstyle pstyle (Poly ps) ctx =
  case uncons ps of
    Nothing -> pure unit
    Just ht -> void do
      beginPath ctx
      setStrokeStyle lstyle.style ctx
      setLineWidth lstyle.width ctx
      moveTo ctx (p2rec ht.head).x (p2rec ht.head).y
      for_ ht.tail \p -> lineTo ctx (p2rec p).x (p2rec p).y
      lineTo ctx (p2rec ht.head).x (p2rec ht.head).y
      stroke ctx
      for_ ps \p -> drawPoint pstyle p ctx

drawValidity :: forall e. FillStyle -> P2 Number -> Plane Boolean -> Number -> Context2D -> Eff (canvas :: CANVAS | e) Unit
drawValidity style (P2 lo) validity gridSz ctx = do
  for_ (enumerate validity) \row ->
    for_ (enumerate row.elem) \p ->
      unless p.elem <<< void $ do
        setFillStyle style ctx
        fillRect ctx { x: lo.x + (toNumber p.idx) * gridSz
                     , y: lo.y + (toNumber (-row.idx)) * gridSz
                     , w: gridSz
                     , h: (-gridSz) }

drawEditPoint :: forall e. LineStyle -> PointStyle -> EditPoint Number -> Context2D -> Eff (canvas :: CANVAS | e ) Unit
drawEditPoint lstyle pstyle { before: P2 {x: bx, y: by}
                            , point:  p@(P2 {x: px, y: py})
                            , after:  P2 {x: ax, y: ay} } ctx = do
  beginPath ctx
  setStrokeStyle lstyle.style ctx
  setLineWidth lstyle.width ctx
  moveTo ctx bx by
  lineTo ctx px py
  lineTo ctx ax ay
  stroke ctx
  drawPoint pstyle p ctx

drawWorkArea :: forall e. Editor -> Eff (canvas :: CANVAS | e) Unit
drawWorkArea { style, view, workArea, context: ctx } = do
  clearRect ctx { x: 0.0, y: 0.0, w: view.width, h: view.height }
  when (view.zoom > style.subGridMinZoom) $
    drawGrid style.subGrid view subGridSz ctx
  drawGrid style.mainGrid view mainGridSz ctx
  when style.drawOrigin $
    drawPoint style.origin view.origin ctx
  case workArea.validity of
    Nothing -> pure unit
    Just val -> drawValidity style.invalidFaceStyle (fromGrid' val.lo) val.validity mainGridSz ctx
  drawPoly style.edge style.point poly' ctx
  case workArea.point of
    Nothing -> pure unit
    Just ep -> drawEditPoint style.edge style.point { before: fromGrid' ep.before
                                                    , point: fromGrid' ep.point
                                                    , after: fromGrid' ep.after } ctx
  where subGridSz = subGridSize view
        mainGridSz = mainGridSize view
        poly' = fromGrid' <$> workArea.poly
        fromGrid' = fromGrid view
