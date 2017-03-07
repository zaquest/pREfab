module Drag.EditPoint where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.Foldable (foldMap)

import Linear.R2 (P2)
import Linear.Metric (distance) as LM
import Data.Polygon ( Poly2, corners, edges, insertAt, updateAt
                    , prunePoly )
import Data.Segment (Seg(..))
import Data.Segment (distance) as Seg
import Data.Corner (Corner(..))
import Editor.Editor (Editor)
import Grid (CGrid)
import Editor.View (fromGrid, toGrid)
import Editor.WorkArea (EditPoint, mkWorkArea)
import Drag.Drag (Drag)
import Utils (enumerate)

onPoly :: Number
       -> Poly2 Number
       -> P2 Number
       -> Maybe { point :: EditPoint Number
                , finish :: P2 Number -> Poly2 Number }
onPoly dist poly p = unwrap (firstPoint <> firstEdge)
  where
    firstPoint = foldMap (First <<< point) (enumerate (corners poly))
    firstEdge = foldMap (First <<< edge) (enumerate (edges poly))
    point {idx, elem: Corner a c b} =
      if LM.distance p c < dist
         then Just { point: {before: a, point: p, after: b}
                   , finish: \e -> prune (updateAt (idx+1) e poly) }
         else Nothing
    edge {idx, elem: seg@(Seg a b)} =
      if Seg.distance p seg < dist
         then Just { point: {before: a, point: p, after: b}
                   , finish: \e -> prune (insertAt (idx+1) e poly) }
         else Nothing
    prune editedPoly = fromMaybe editedPoly (prunePoly editedPoly)

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
