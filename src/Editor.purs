module Editor where

import Prelude
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, beginPath, setStrokeStyle, setLineWidth, stroke, moveTo, lineTo, setFillStyle, arc, fill, fillRect)
import Control.Monad.Eff (Eff)
import Data.Int (toNumber, round)
import Math (pi)

import Data.Traversable (for_)
import Linear.R2 (P2(..), p2, p2rec, (^/), (^*))
import Data.Polygon (Poly2, Polygon(..))
import Data.Polygon as Poly
import RedEclipse.Face (validatePoly, ValidityRec, Plane)
import Grid (CGrid)
import Grid as Grid
import Utils (whileM_, enumerate)
import Style (FillStyle, PointStyle, LineStyle, Style, defaultStyle)

data DragPoint = DragPoint { dragBefore :: P2 CGrid
                           , dragPoint :: P2 CGrid
                           , dragAfter :: P2 CGrid
                           , new :: Boolean }

data Editor = Editor { context :: Context
                     , style :: Style
                     , origin :: P2 Number
                     , zoom :: Number
                     , poly :: Poly2 CGrid
                     , point :: Maybe DragPoint
                     , validity :: Maybe ValidityRec }

mkEditor :: Context -> Poly2 CGrid -> Editor
mkEditor ctx poly =
  Editor { context: ctx
         , style: defaultStyle
         , origin: (p2 ctx.width ctx.height) ^/ 2.0
         , zoom: 2.0
         , poly: poly
         , point: Nothing
         , validity: validatePoly poly }

invY :: forall a. Ring a => P2 a -> P2 a
invY (P2 {x, y}) = P2 { x: x, y: (-y) }

fromGrid :: P2 Number -> Number -> P2 CGrid -> P2 Number
fromGrid o gridSz p = o + (toNumber <$> invY p) ^* gridSz

toGrid :: P2 Number -> Number -> P2 Number -> P2 CGrid
toGrid o gridSz p = invY (round <$> ((p - o) ^/ gridSz))

type Context = { context :: Context2D
               , width :: Number
               , height :: Number }

mkContext :: Context2D -> Number -> Number -> Context
mkContext ctx w h = { context: ctx, width: w, height: h }

subGridSize :: Editor -> Number
subGridSize (Editor e) = e.zoom * e.style.subGridSize

mainGridSize :: Editor -> Number
mainGridSize (Editor e) = e.zoom * e.style.subGridSize * (toNumber Grid.gridSize)

drawGrid :: forall e. LineStyle -> P2 Number -> Number -> Context -> Eff (canvas :: CANVAS | e) Unit
drawGrid style origin gridSz { context: ctx, width, height } = void do
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

drawPoint :: forall e. PointStyle -> P2 Number -> Context -> Eff (canvas :: CANVAS | e) Unit
drawPoint style (P2 p) ({ context: ctx }) = void do
  beginPath ctx
  setFillStyle style.style ctx
  arc ctx thePoint
  fill ctx
  where thePoint = { x: p.x, y: p.y, r: style.radius, start: 0.0, end: 2.0 * pi }

drawPoly :: forall e. LineStyle -> PointStyle -> Poly2 Number -> Context -> Eff (canvas :: CANVAS | e) Unit
drawPoly lstyle pstyle (Poly ps) context@{ context: ctx } =
  case Poly.uncons ps of
    Nothing -> pure unit
    Just ht -> void do
      beginPath ctx
      setStrokeStyle lstyle.style ctx
      setLineWidth lstyle.width ctx
      moveTo ctx (p2rec ht.head).x (p2rec ht.head).y
      for_ ht.tail \p -> lineTo ctx (p2rec p).x (p2rec p).y
      lineTo ctx (p2rec ht.head).x (p2rec ht.head).y
      stroke ctx
      for_ ps \p -> drawPoint pstyle p context

drawValidity :: forall e. FillStyle -> P2 Number -> Plane Boolean -> Number -> Context -> Eff (canvas :: CANVAS | e) Unit
drawValidity style (P2 lo) validity gridSz { context: ctx } = do
  for_ (enumerate validity) \row ->
    for_ (enumerate row.elem) \p ->
      unless p.elem <<< void $ do
        setFillStyle style ctx
        fillRect ctx { x: lo.x + (toNumber p.idx) * gridSz
                     , y: lo.y + (toNumber (-row.idx)) * gridSz
                     , w: gridSz
                     , h: (-gridSz) }

drawWorkArea :: forall e. Editor -> Eff (canvas :: CANVAS | e) Unit
drawWorkArea ed@(Editor e) = do
  when (e.zoom > e.style.subGridMinZoom) $
    drawGrid e.style.subGrid e.origin subGridSz e.context
  drawGrid e.style.mainGrid e.origin mainGridSz e.context
  when e.style.drawOrigin $
    drawPoint e.style.origin e.origin e.context
  case e.validity of
    Nothing -> pure unit
    Just val -> drawValidity e.style.invalidFaceStyle (fromGrid e.origin subGridSz val.lo) val.validity mainGridSz e.context
  drawPoly e.style.edge e.style.point poly' e.context
  where subGridSz = subGridSize ed
        mainGridSz = mainGridSize ed
        poly' = map (fromGrid e.origin subGridSz) e.poly
