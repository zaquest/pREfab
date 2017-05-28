module Editor.Render where

import Prelude
import Control.Monad.Eff (Eff)
import Math (pi)
import Data.Int (toNumber, round)
import Data.Traversable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas ( CANVAS, Context2D, beginPath, setStrokeStyle
                       , setLineWidth, stroke, moveTo, lineTo
                       , setFillStyle, arc, fill, fillRect, clearRect )

import Linear.R2 (P2(..), r2)
import Linear.Vector ((^*), (^/))
import Data.Polygon (Poly2, uncons, points)
import RedEclipse.Face (Plane)
import Editor.Style (FillStyle, PointStyle, LineStyle)
import Editor.Editor (Editor)
import Editor.View (View, fromGrid, subGridSize, mainGridSize)
import Editor.WorkArea (EditPoint)
import Utils (enumerate, whileM_)

drawGrid :: forall e. LineStyle -> View -> Number -> Context2D -> Eff (canvas :: CANVAS | e) Unit
drawGrid style { origin, width, height } gridSz ctx = void do
  _ <- beginPath ctx
  _ <- setStrokeStyle style.style ctx
  _ <- setLineWidth style.width ctx
  whileM_ start.x (\x -> x <= width) $ \x -> do
    _ <- moveTo ctx x 0.0
    _ <- lineTo ctx x height
    pure (x + gridSz)
  whileM_ start.y (\y -> y <= height) $ \y -> do
    _ <- moveTo ctx 0.0 y
    _ <- lineTo ctx width y
    pure (y + gridSz)
  stroke ctx
  where start = r2 $ origin - ((toNumber <<< round) <$> (origin ^/ gridSz)) ^* gridSz

drawPoint :: forall e. PointStyle -> P2 Number -> Context2D -> Eff (canvas :: CANVAS | e) Unit
drawPoint style (P2 p) ctx = void do
  _ <- beginPath ctx
  _ <- setFillStyle style.style ctx
  _ <- arc ctx thePoint
  fill ctx
  where thePoint = { x: p.x, y: p.y, r: style.radius, start: 0.0, end: 2.0 * pi }

drawPoly :: forall e. LineStyle -> PointStyle -> Poly2 Number -> Context2D -> Eff (canvas :: CANVAS | e) Unit
drawPoly lstyle pstyle poly ctx = do
  let ht = uncons poly
  let h = r2 ht.head
  _ <- beginPath ctx
  _ <- setStrokeStyle lstyle.style ctx
  _ <- setLineWidth lstyle.width ctx
  _ <- moveTo ctx h.x h.y
  for_ ht.tail \(P2 p) -> lineTo ctx p.x p.y
  _ <- lineTo ctx h.x h.y
  _ <- stroke ctx
  for_ (points poly) \p -> drawPoint pstyle p ctx

drawValidity :: forall e. FillStyle -> P2 Number -> Plane Boolean -> Number -> Context2D -> Eff (canvas :: CANVAS | e) Unit
drawValidity style (P2 lo) validity gridSz ctx = do
  for_ (enumerate validity) \row ->
    for_ (enumerate row.elem) \p ->
      unless p.elem <<< void $ do
        _ <- setFillStyle style ctx
        fillRect ctx { x: lo.x + (toNumber p.idx) * gridSz
                     , y: lo.y + (toNumber (-row.idx)) * gridSz
                     , w: gridSz
                     , h: (-gridSz) }

drawEditPoint :: forall e. LineStyle -> PointStyle -> EditPoint Number -> Context2D -> Eff (canvas :: CANVAS | e ) Unit
drawEditPoint lstyle pstyle { before: P2 {x: bx, y: by}
                            , point:  p@(P2 {x: px, y: py})
                            , after:  P2 {x: ax, y: ay} } ctx = do
  _ <- beginPath ctx
  _ <- setStrokeStyle lstyle.style ctx
  _ <- setLineWidth lstyle.width ctx
  _ <- moveTo ctx bx by
  _ <- lineTo ctx px py
  _ <- lineTo ctx ax ay
  _ <- stroke ctx
  drawPoint pstyle p ctx

drawWorkArea :: forall e. Editor -> Eff (canvas :: CANVAS | e) Unit
drawWorkArea { style, view, workArea, context: ctx } = do
  _ <- clearRect ctx { x: 0.0, y: 0.0, w: view.width, h: view.height }
  when (view.zoom > style.subGridMinZoom) $
    drawGrid style.subGrid view subGridSz ctx
  drawGrid style.mainGrid view mainGridSz ctx
  when style.drawOrigin $
    drawPoint style.origin view.origin ctx
  let val = workArea.validity
  drawValidity style.invalidFaceStyle (fromGrid' val.lo) val.validity mainGridSz ctx
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
