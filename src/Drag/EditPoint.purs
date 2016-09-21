module Drag.EditPoint where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.First (First(..), runFirst)
import Control.MonadZero (guard)
import Math (sqrt, abs)
import Data.Array (snoc, uncons, last, init, cons, zipWith, fromFoldable)
import Data.Foldable (foldMap)

import Linear.R2 (P2(..), distance)
import Data.Polygon ( Poly2, Polygon(..), update, length, elemIndex, Edge2
                    , Edge(..), Line2, Line(..), edges, insert, simplify )
import Editor.Editor (Editor)
import Grid (CGrid)
import Editor.View (fromGrid, toGrid)
import Editor.WorkArea (EditPoint, mkWorkArea)
import Drag.Drag (Drag)
import Linear.Epsilon ((~=))
import Trace (trace)

lineDistance :: P2 Number -> Line2 Number -> Maybe Number
lineDistance (P2 {x: x2, y: y2}) (Line (P2 {x: x0, y: y0}) (P2 {x: x1, y: y1})) =
  let x = x1 - x0
      y = y1 - y0
      denom = sqrt (x * x + y * y)
  in if denom ~= 0.0
       then Nothing
       else Just (abs (x * (y2 - y0) - (x2 - x0) * y) / denom)

onEdge :: Number -> P2 Number -> Edge2 Number -> Maybe (EditPoint Number)
onEdge dist p@(P2 {x: px, y: py}) (Edge b@(P2 {x: bx, y: by}) a@(P2 {x: ax, y: ay})) = do
  d <- lineDistance p (Line b a)
  guard (sameSquare && d < dist)
  pure { before: b, point: p, after: a }
  where sameSquare = (bx - dist < px && px < ax + dist
                      || ax - dist < px && px < bx + dist)
                     &&
                     (by - dist < py && py < ay + dist
                      || ay - dist < py && py < by + dist)

onPoint :: Number -> P2 Number -> EditPoint Number -> Maybe (EditPoint Number)
onPoint dist p ep@{ point } =
  if distance p point < dist
    then Just ep
    else Nothing

polyEditPoints :: forall a. Poly2 a -> Array (EditPoint a)
polyEditPoints (Poly ps') = fromMaybe [] do
  guard (length ps' > 2)
  {head: h, tail: t} <- uncons ps
  l <- last ps
  i <- init ps
  pure $ zipWith (\b pa -> {before: b, point: pa.p, after: pa.a})
                  (cons l i)
                  (zipWith (\p a -> {p: p, a: a}) ps (snoc t h))
  where ps = fromFoldable ps'

onPoly :: Number
       -> Poly2 Number
       -> P2 Number
       -> Maybe { point :: EditPoint Number
                , finish :: P2 Number -> Poly2 Number }
onPoly dist poly@(Poly ps') p = runFirst (First firstPoint <> First firstEdge)
  where onPoints = (onPoint dist p) <$> polyEditPoints poly
        onEdges = (onEdge dist p) <$> fromFoldable (edges poly)
        firstOf = runFirst <<< foldMap First
        firstPoint = (\ep -> { point: ep, finish: pointFinish ep }) <$> firstOf onPoints
        firstEdge = (\ep -> { point: ep, finish: edgeFinish ep }) <$> firstOf onEdges
        pointFinish { before } p = fromMaybe poly (simplify' <$> do
          idx <- elemIndex before ps'
          update ((idx + 1) `mod` (length ps')) p poly)
        edgeFinish { before } p = fromMaybe poly (simplify' <$> do
          idx <- elemIndex before ps'
          pure (insert (idx + 1) p poly))
        simplify' origPoly@(Poly ps) =
          let sps = simplify ps
          in if length sps < 3
               then origPoly
               else Poly sps

editPointDrag :: Editor -> P2 Number -> Maybe Drag
editPointDrag e start = do
  let poly' = fromGrid e.view <$> e.workArea.poly
  ep' <- onPoly e.style.point.radius poly' start
  let ep = { before: toGrid' ep'.point.before
           , point:  toGrid' ep'.point.point
           , after:  toGrid' ep'.point.after }
  let e' = e { workArea = e.workArea { point = Just ep } }
  pure { move: moveEdit e'
       , stop: stopEdit e' (map toGrid' <<< ep'.finish) }
  where toGrid' = toGrid e.view


moveEdit :: Editor -> P2 Number -> Editor
moveEdit e current =
  case e.workArea.point of
    Nothing -> e
    Just point -> e { workArea = e.workArea { point = Just (point { point = toGrid' current }) } }
  where toGrid' = toGrid e.view

stopEdit :: Editor -> (P2 Number -> Poly2 CGrid) -> P2 Number -> Editor
stopEdit e finish current = e { workArea = mkWorkArea (finish current) }
