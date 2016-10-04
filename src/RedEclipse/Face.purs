module RedEclipse.Face where
--     ( validatePoly
--     , Validity
--     , Plane(..)
--     , toPlane
--     --, CFace
--     --, gridPrev
--     --, gridNext
--     ) where

import Prelude
import Control.MonadZero (guard)
import Data.Function (on)
import Data.Int (ceil, round, toNumber)
import Data.Foldable (class Foldable, minimumBy, maximumBy, foldl)
import Data.Monoid ((<>))
import Data.Traversable (class Traversable, traverseDefault, sequence)
import Data.Array ( (..), cons, snoc, filter, zipWith, span, elemIndex
                  , reverse, (!!), head, (\\), intersect )
import Data.Array (length, uncons) as A
import Data.Polygon ( Poly2, isConvex, length, origin, square, clip
                    , move, prunePoly, points, clockWise
                    , uncons, mkPoly, edges )
import Data.BoundingBox (boundingBox, boundingRect)
import Linear.R2 (P2(..), p2, r2, V2(..), v2, R2Rec)
import Linear.Vector ((^*))
import Linear.Epsilon (class Epsilon, (~=))
import Linear.Metric (qd)
import Data.Segment (Seg2, Seg(..), ends)
import Data.Maybe (Maybe(..), maybe)
import Grid (CGrid, gridSize, gridPrev, gridNext)
import RedEclipse.Face.Side (Side(..), allSides)
import RedEclipse.Cube (Cube(..), Edge(..), emptyEdge, solidEdge, emptyCube)
import Utils (fromJust, bug)
import Data.Ord (abs)
import Trace (trace)

-- | Face coordinate type. In range [0, 8]
type CFace = Int

type Corner = P2 Side

allCorners :: Array Corner
allCorners = [p2 SDown SLeft, p2 SUp SLeft, p2 SUp SRight, p2 SDown SRight]

type ASide a = { side :: Side, edge :: Seg2 a }

-- overEdge :: forall a b. ASide a -> (Line2 a -> Line2 b) -> ASide b
-- overEdge (aSide@{ edge: anEdge }) f = aSide { edge = f anEdge }

-- type ACorner a = { corner :: Corner, point :: P2 a }

-- data Edge = Edge { start :: CFace, end :: CFace }

-- edge :: CFace -> CFace -> Edge
-- edge s e = Edge { start: s, end: e }

-- emptyEdge :: Edge
-- emptyEdge = edge 0 0

-- data Face = Face { fUp    :: Edge
--                  , fRight :: Edge
--                  , fDown  :: Edge
--                  , fLeft  :: Edge }
-- 
-- face :: Edge -> Edge -> Edge -> Edge -> Face
-- face u r d l = Face { fUp: u, fRight: r, fDown: d, fLeft: l }
-- 
-- emptyFace :: Face
-- emptyFace = face emptyEdge emptyEdge emptyEdge emptyEdge

-- | Face is valid if it has 3 or 4 edges and it's convex and all of
-- it's vertices lie on grid node.
validFace :: Poly2 Number -> Boolean
validFace oPoly =
  case prunePoly oPoly of
    Nothing -> true
    Just poly ->
      let len = length poly
          has3Or4Edges = len == 3 || len == 4
          onGridAndConvex =
            let o = (gridPrev <<< ceil) <$> origin poly
                mpoly' = sequence $ toFaceCoords o <$> poly
             in Just true == (isConvex <$> mpoly')
        in has3Or4Edges && onGridAndConvex

type Plane a = Array (Array a)

type Validity = { lo :: P2 CGrid, hi :: P2 CGrid, validity :: Plane Boolean }

-- | Polygon is valid if all of it's intersections with big (8x8) gird
-- are valid faces
validatePoly :: Poly2 CGrid -> Validity
validatePoly poly =
  let bb = boundingBox poly
      plo = gridPrev <$> bb.lo
      lo = r2 plo
      phi = gridNext <$> bb.hi
      hi = r2 phi
      poly' = move poly (negate (V2 lo))
      width = hi.x - lo.x
      height = hi.y - lo.y
      oPoly = map toNumber <$> poly'
      validity = (0..((height / gridSize) - 1)) <#> \y ->
                   (0..((width / gridSize) - 1)) <#> \x ->
                     let cs = square (toNumber gridSize) (toNumber <$> p2 x y ^* gridSize)
                      in maybe true validFace (clip cs oPoly)
  in { lo: plo, hi: phi, validity: validity }

-- | Polygon is valid if all of it's intersections with big (8x8) gird
-- are valid faces
toPlane :: Poly2 CGrid -> Maybe { size :: V2 Int, plane :: Plane Cube }
toPlane poly =
  let bb = boundingBox poly
      plo = gridPrev <$> bb.lo
      lo = r2 plo
      phi = gridNext <$> bb.hi
      hi = r2 phi
      poly' = move poly (negate (V2 lo))
      width = hi.x - lo.x
      height = hi.y - lo.y
      oPoly = map toNumber <$> poly'
      plane =
        sequence $ (0..((width / gridSize) - 1)) <#> \x ->
          sequence $ (0..((height / gridSize) - 1)) <#> \y ->
            let cs = square (toNumber gridSize) (toNumber <$> p2 (trace x) (trace y) ^* gridSize)
             in case clip cs oPoly >>= prunePoly of
                  Nothing -> Just emptyCube
                  Just poly -> (trace $ faceToCube <$> polyToFace (trace poly))
   in (\p -> { size: v2 (width / gridSize) (height / gridSize), plane: p }) <$> plane


-- | Convert point `p` to face coordinates. Face coordinates are
-- coordinates starting at lower left corner of the face (or face's
-- origin), they are in range [0, 8] along both x and y axes.
-- `o` - face's origin.
-- Fails if `p` is not in the face given by `o` or when `p` is not
-- on the 8x8 grid (hence not representable with cube's face).
--
-- The name might be misleading since function does not convert to the
-- CFace. It is not converted to the CFace type because further
-- processing is required.
toFaceCoords :: P2 CGrid -> P2 Number -> Maybe (P2 CFace)
toFaceCoords o p = if onGrid && inFace (r2 f).x && inFace (r2 f).y
                      then Just f
                      else Nothing
  where
    p' = round <$> p
    f = p' - o
    onGrid = p ~= (toNumber <$> p')
    inFace z = 0 <= z && z <= gridSize

newtype Face a = Face { up :: a, right :: a, down :: a, left :: a }

instance showFace :: Show a => Show (Face a) where
  show (Face f) = "(Face { up: " <> show f.up <> ", right: " <> show f.right <> ", down: " <> show f.down <> ", left: " <> show f.left <> ")"

instance functorFace :: Functor Face where
  map g (Face f) = Face { up: g f.up, right: g f.right
                        , down: g f.down, left: g f.left }

instance foldableFace :: Foldable Face where
  foldMap fm (Face f) = fm f.up <> fm f.right <> fm f.down <> fm f.left
  foldr g acc (Face f) = g f.up (g f.right (g f.down (g f.left acc)))
  foldl g acc (Face f) = g (g (g (g acc f.up) f.right) f.down) f.left

instance traversableFace :: Traversable Face where
  sequence (Face f) = face <$> f.up <*> f.right <*> f.down <*> f.left
  traverse = traverseDefault

face :: forall a. a -> a -> a -> a -> Face a
face up right down left = Face { up, right, down, left }

uniFace :: forall a. a -> Face a
uniFace a = face a a a a

emptyFace :: Face Edge
emptyFace = uniFace emptyEdge

polyToFace :: Poly2 Number -> Maybe (Face Edge)
polyToFace poly = do
  -- Validate poly and get on-grid poly `Poly2 CGrid`
  let len = length poly
  guard (len == 3 || len == 4)
  let o = (gridPrev <<< ceil) <$> origin poly
  gPoly <- sequence $ toFaceCoords o <$> poly
  guard (isConvex gPoly)
  let es = edges gPoly
  let ps = points gPoly
  aFace <- foldl (setEdge' ps) (Just $ uniFace Nothing) es
  let face' = if len == 3 then addMissing aFace else aFace
  let face'' = foldl (\f s -> setSide f s (segToEdge s <$> getSide face' s)) (uniFace Nothing) allSides
  sequence face''

setEdge' :: Array (P2 CGrid) -> Maybe (Face (Maybe (Seg2 CGrid))) -> Seg2 CGrid -> Maybe (Face (Maybe (Seg2 CGrid)))
setEdge' _ Nothing _ = Nothing
setEdge' ps (Just f) seg@(Seg s e) =
  let pt = fromJust $ head (ps \\ [s, e])
      side = assignSide ((\p -> toNumber <$> p) <$> seg) (toNumber <$> pt)
   in setEdge f side seg

addMissing :: Face (Maybe (Seg2 CGrid)) -> Face (Maybe (Seg2 CGrid))
addMissing (Face f@{up: Nothing, left: Just (Seg s1 e1), right: Just (Seg s2 e2)}) =
  let p = fromJust <<< head $ intersect [s1, e1] [s2, e2]
   in Face f { up = Just (Seg p p) }
addMissing (Face f@{right: Nothing, up: Just (Seg s1 e1), down: Just (Seg s2 e2)}) =
  let p = fromJust <<< head $ intersect [s1, e1] [s2, e2]
   in Face f { right = Just (Seg p p) }
addMissing (Face f@{down: Nothing, left: Just (Seg s1 e1), right: Just (Seg s2 e2)}) =
  let p = fromJust <<< head $ intersect [s1, e1] [s2, e2]
   in Face f { down = Just (Seg p p) }
addMissing (Face f@{left: Nothing, up: Just (Seg s1 e1), down: Just (Seg s2 e2)}) =
  let p = fromJust <<< head $ intersect [s1, e1] [s2, e2]
   in Face f { left = Just (Seg p p) }
addMissing f = f

-- | Input: an `edge` to which side is being assigned and a `point`
-- of a polygon that doesn't belong to the `edge`.
-- The idea is to first choose wether `edge` is horizontal or vertical
-- by checking lengths of it's projections of Ox and Oy axes.
-- And then for horizontal check if the `point` lies below or above
-- the `edge` and for vertical if the `point` is on the left or on the
-- right.
assignSide :: Seg2 Number -> P2 Number -> Side
assignSide seg@(Seg (P2 s) (P2 e)) (P2 p) =
  if proj _.x seg >= proj _.y seg
     then -- horizontal
       let y = (p.x - s.x) / (e.x - s.x) * (e.y - s.y) + s.y
        in if p.y < y then SUp else SDown
     else -- vertical
       let x = (p.y - s.y) / (e.y - s.y) * (e.x - s.x) + s.x
        in if p.x < x then SRight else SLeft
  where proj g (Seg (P2 s) (P2 e)) = abs (g s - g e)

-- | Fails if side has already been set
setEdge :: Face (Maybe (Seg2 CGrid)) -> Side -> Seg2 CGrid -> Maybe (Face (Maybe (Seg2 CGrid)))
setEdge f side seg =
  case getSide f side of
    Nothing -> Just (setSide f side (Just seg))
    Just _ -> Nothing

getSide :: forall a. Face a -> Side -> a
getSide (Face f) SUp = f.up
getSide (Face f) SRight = f.right
getSide (Face f) SDown = f.down
getSide (Face f) SLeft = f.left

setSide :: forall a. Face a -> Side -> a -> Face a
setSide (Face f) SUp val = Face f { up = val }
setSide (Face f) SRight val = Face f { right = val }
setSide (Face f) SDown val = Face f { down = val }
setSide (Face f) SLeft val = Face f { left = val }

segToEdge :: Side -> Seg2 CGrid -> Edge
segToEdge side (Seg (P2 so) (P2 eo))
  | isHoriz side = horiz so eo
  | otherwise = vert so eo

horiz :: R2Rec CGrid -> R2Rec CGrid -> Edge
horiz so eo = if so.x > eo.x
                 then Edge { offset: gridSize - so.x
                           , limit: gridSize - eo.x }
                 else Edge { offset: gridSize - eo.x
                           , limit: gridSize - so.x }

vert :: R2Rec CGrid -> R2Rec CGrid -> Edge
vert so eo = if so.y < eo.y
                then Edge { offset: so.y, limit: eo.y }
                else Edge { offset: eo.y, limit: so.y }

isHoriz :: Side -> Boolean
isHoriz SUp = true
isHoriz SDown = true
isHoriz _ = false

--polyToFace :: Poly2 Number -> Maybe (Face Edge)
--polyToFace poly = do
--  -- Validate poly and get on-grid poly `Poly2 CGrid`
--  let len = length poly
--  guard (len == 3 || len == 4)
--  let o = (gridPrev <<< ceil) <$> origin poly
--  gPoly <- sequence $ toFaceCoords o <$> poly
--  guard (isConvex gPoly)
--  let ht = uncons gPoly
--  let gPoly' = fromJust $ mkPoly (cons ht.head (points gPoly))
--  -- Convert on-grid poly `gPoly :: Poly2 CGrid` to `Face`
--  let qds = edges gPoly' <#> \(Seg s e) ->
--            let qd' = sideQd s e
--             in { edge: Seg s e
--                , up:    qd' (p2 8 8) (p2 0 8)
--                , right: qd' (p2 8 0) (p2 8 8)
--                , down:  qd' (p2 8 0) (p2 0 0)
--                , left:  qd' (p2 0 0) (p2 0 8) }
--  pure (theFace qds)
--
--  where
--
--    theFace qds =
--      let up = fromJust $ minimumBy (compare `on` _.up) qds
--          qds' = filter (\q -> q.edge /= up.edge) qds
--          right = fromJust $ minimumBy (compare `on` _.right) qds'
--          qds'' = filter (\q -> q.edge /= right.edge) qds'
--          down = fromJust $ minimumBy (compare `on` _.down) qds''
--          qds''' = filter (\q -> q.edge /= down.edge) qds''
--          left = fromJust $ minimumBy (compare `on` _.left) qds'''
--       in Face { up: segToEdge SUp up.edge
--               , right: segToEdge SRight right.edge
--               , down: segToEdge SDown down.edge
--               , left: segToEdge SLeft left.edge }
--
--
--    -- so - edge start translated by poly origin
--    -- eo - edge end translated by poly origin
--    -- s - side start
--    -- e - side end
--
--    -- Finds minimum sum of quadrances between points in
--    -- {so, eo} x {s, e}. This is a measure of how close a poly edge
--    -- `Seg so eo` to a face side `Seg s e`.
--    sideQd so eo s e = let forth = qd s so + qd e eo
--                           back = qd s eo + qd e so
--                        in min forth back
--
--    segToEdge :: Side -> Seg2 CGrid -> Edge
--    segToEdge side (Seg (P2 so) (P2 eo))
--      | isHoriz side = horiz so eo
--      | otherwise = vert so eo
--
--    horiz so eo = if so.x > eo.x
--                     then Edge { offset: gridSize - so.x
--                               , limit: gridSize - eo.x }
--                     else Edge { offset: gridSize - eo.x
--                               , limit: gridSize - so.x }
--
--    vert so eo = if so.y < eo.y
--                    then Edge { offset: so.y, limit: eo.y }
--                    else Edge { offset: eo.y, limit: so.y }
--
--    isHoriz :: Side -> Boolean
--    isHoriz SUp = true
--    isHoriz SDown = true
--    isHoriz _ = false

faceToCube :: Face Edge -> Cube
faceToCube (Face f) =
  Cube { edge01: f.down   , edge23: f.down
       , edge45: f.up     , edge67: f.up
       , edge02: solidEdge, edge46: solidEdge
       , edge13: solidEdge, edge57: solidEdge
       , edge04: f.right  , edge15: f.left
       , edge26: f.right  , edge37: f.left }

-- old impl ported from haskell
-- polyToFace :: Poly2 Number -> Maybe (Face Edge)
-- polyToFace poly = do
--   let len = length poly
--   guard $ len == 3 || len == 4
--   let o = (gridPrev <<< ceil) <$> origin poly
--   gPoly <- sequence $ toFaceCoords o <$> poly
--   guard (isConvex gPoly)
--   let urdl = map (\se -> toEdge se.side se.edge) $ findSides gPoly
--   pure (fromJust $ face <$> (urdl !! 0) <*> (urdl !! 1) <*> (urdl !! 2) <*> (urdl !! 3))
--   where ps = points poly
-- 
-- -- | Converts line in normal coordinates (from down to up, form left
-- -- to right) into RE's edge system with it's coordinates.
-- toEdge :: Side -> Seg2 Int -> Edge
-- toEdge side (Seg (P2 s) (P2 e))
--   | side == SUp || side == SDown = Edge { offset: gridSize - e.x, limit: gridSize - s.x }
--   | otherwise                    = Edge { offset: s.y, limit: e.y }
-- 
-- findSides :: Poly2 Int -> Array {side :: Side, edge :: Seg2 Int}
-- findSides poly =
--   let ps' = points poly
--       ps = if A.length ps' == 3 then insertMissing ps' else ps'
--       closest = zipWith (\p c -> {point: p, corner: c.corner, minDist: c.minDist}) ps (map (\p -> findClosest p allCs) ps)
--       cpd = fromJust $ minimumBy (compare `on` _.minDist) closest
--       pidx = fromJust (elemIndex cpd.point ps)
--       clock = clockWise poly
--       dirCs = if clock then allCorners else reverse allCorners
--       cidx = fromJust (elemIndex cpd.corner dirCs)
--       cs = rotate ((abs (cidx - pidx)) `mod` 4) dirCs
--       corners = zipWith (\c p -> {corner: c, point: p}) cs ps
--   in map (combineEdge corners) allSides
--   where
--     br = points (boundingRect poly)
--     allCs = zipWith (\c p -> {corner: c, point: p}) allCorners br
-- 
--     insertMissing ps =
--       let closest = zipWith (\p c -> {point: p, corner: c.corner, minDist: c.minDist}) ps (map (\p -> findClosest p allCs) ps)
--           cpd = fromJust $ maximumBy (compare `on` _.minDist) closest
--           ir = span (\p -> p == cpd.point) ps
--        in ir.init <> (cons cpd.point ir.rest)
-- 
--     -- | Takes a point and a list of points and returns a list of
--     -- points that are closest to the point.
--     findClosest :: forall a. (Epsilon a, Ord a, Ring a) => P2 a -> Array {corner :: Corner, point :: P2 a} -> {corner :: Corner, minDist :: a}
--     findClosest p ps = fromJust $ minimumBy (compare `on` _.minDist) (map (\cp -> {corner: cp.corner, minDist: qd p cp.point}) ps)
-- 
--     rotate :: forall a. Int -> Array a -> Array a
--     rotate 0 xs = xs
--     rotate n xs = let ht = fromJust (A.uncons xs)
--                    in rotate (n-1) (snoc ht.tail ht.head)
-- 
-- -- | Swaps `start` and `end` of a line if they don't match a `side`
-- -- orientation.
-- fixOrientation :: forall a. Ord a => Side -> Seg2 a -> Seg2 a
-- fixOrientation side (Seg (P2 s) (P2 e))
--   | side == SUp || side == SDown = if s.x < e.x then Seg (P2 s) (P2 e) else Seg (P2 e) (P2 s)
--   | otherwise                    = if s.y < e.y then Seg (P2 s) (P2 e) else Seg (P2 e) (P2 s)
-- 
-- combineEdge :: forall a. Ord a => Array {corner :: P2 Side, point :: P2 a} -> Side -> {side :: Side, edge :: Seg2 a}
-- combineEdge corners side =
--   let ps = map _.point $ filter ((\(P2 s) -> side == s.x || side == s.y) <<< (\cp -> cp.corner)) corners
--    in case ps of
--         [p1, p2] -> {side, edge: fixOrientation side (Seg p1 p2)}
--         ps'      -> bug $ "combineEdges: wrong number of points (" <> show (A.length ps') <> ") for the side " <> show side
