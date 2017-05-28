module RedEclipse.Face
     ( validatePoly
     , Validity
     , Plane(..)
     , toPlane
     ) where

import Prelude
import Control.MonadZero (guard)
import Data.Int (ceil, round, toNumber)
import Data.Foldable (class Foldable, foldl)
import Data.Traversable (class Traversable, traverseDefault, sequence)
import Data.Array ((..), head, (\\), intersect)
import Data.Polygon ( Poly2, isConvex, length, origin, square, clip
                    , move, prunePoly, points, edges )
import Data.BoundingBox (width, height, boundingBox)
import Linear.R2 (P2(..), p2, r2, V2(..), v2, R2Rec)
import Linear.Vector ((^*))
import Linear.Epsilon ((~=))
import Data.Segment (Seg2, Seg(..))
import Data.Maybe (Maybe(..), maybe)
import Grid (CGrid, gridSize, gridPrev, gridNext)
import RedEclipse.Face.Side (Side(..), allSides)
import RedEclipse.Cube (Cube(..), Edge(..), emptyEdge, solidEdge, emptyCube)
import Utils (fromJust)
import Data.Ord (abs)

-- | Face coordinate type. In range [0, 8]
type CFace = Int

type Corner = P2 Side

allCorners :: Array Corner
allCorners = [p2 SDown SLeft, p2 SUp SLeft, p2 SUp SRight, p2 SDown SRight]

type ASide a = { side :: Side, edge :: Seg2 a }

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
            let cs = square (toNumber gridSize) (toNumber <$> p2 x y ^* gridSize)
             in case clip cs oPoly >>= prunePoly of
                  Nothing -> Just emptyCube
                  Just poly'' -> faceToCube <$> polyToFace poly''
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
  aFace <- foldl (setEdge' gPoly) (Just $ uniFace Nothing) es
  let face' = if len == 3 then addMissing aFace else aFace
  let face'' = foldl (\f s -> setSide f s (segToEdge s <$> getSide face' s)) (uniFace Nothing) allSides
  sequence face''

setEdge' :: Poly2 CGrid -> Maybe (Face (Maybe (Seg2 CGrid))) -> Seg2 CGrid -> Maybe (Face (Maybe (Seg2 CGrid)))
setEdge' _ Nothing _ = Nothing
setEdge' poly (Just f) seg@(Seg s e) =
  let ps = points poly
      bb = boundingBox poly
      pt = fromJust $ head (ps \\ [s, e])
      bbSize = { width: toNumber (width bb), height: toNumber (height bb) }
      side = assignSide bbSize (map toNumber <$> seg) (toNumber <$> pt)
   in setEdge f side seg

commonPoint :: Seg2 CGrid -> Seg2 CGrid -> Maybe (P2 CGrid)
commonPoint (Seg s1 e1) (Seg s2 e2) = head (intersect [s1, e1] [s2, e2])

addMissing :: Face (Maybe (Seg2 CGrid)) -> Face (Maybe (Seg2 CGrid))
addMissing (Face f@{up: Nothing, left: Just l, right: Just r}) =
  let p = fromJust (commonPoint l r)
   in Face f { up = Just (Seg p p) }
addMissing (Face f@{right: Nothing, up: Just u, down: Just d}) =
  let p = fromJust (commonPoint u d)
   in Face f { right = Just (Seg p p) }
addMissing (Face f@{down: Nothing, left: Just l, right: Just r}) =
  let p = fromJust (commonPoint l r)
   in Face f { down = Just (Seg p p) }
addMissing (Face f@{left: Nothing, up: Just u, down: Just d}) =
  let p = fromJust (commonPoint u d)
   in Face f { left = Just (Seg p p) }
addMissing f = f

-- | Input: an `edge` to which side is being assigned and a `point`
-- of a polygon that doesn't belong to the `edge`.
-- The idea is to first choose wether `edge` is horizontal or vertical
-- by checking ratio of lengths of it's projections on Ox and Oy axes
-- to width and height of polygon's bounding box.
-- And then for horizontal check if the `point` lies below or above
-- the `edge` and for vertical if the `point` is on the left or on the
-- right.
assignSide :: { width :: Number, height :: Number }
           -> Seg2 Number
           -> P2 Number
           -> Side
assignSide {width, height} seg@(Seg (P2 s) (P2 e)) (P2 p) =
  if rpx > rpy || rpx ~= rpy && px > py
     then -- horizontal edge
       let y = (p.x - s.x) / (e.x - s.x) * (e.y - s.y) + s.y
        in if p.y < y then SUp else SDown
     else -- vertical edge
       let x = (p.y - s.y) / (e.y - s.y) * (e.x - s.x) + s.x
        in if p.x < x then SRight else SLeft
  where proj g (Seg (P2 s') (P2 e')) = abs (g s' - g e')
        px = proj _.x seg
        py = proj _.y seg
        rpx = px / width
        rpy = py / height

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

faceToCube :: Face Edge -> Cube
faceToCube (Face f) =
  Cube { edge01: f.down   , edge23: f.down
       , edge45: f.up     , edge67: f.up
       , edge02: solidEdge, edge46: solidEdge
       , edge13: solidEdge, edge57: solidEdge
       , edge04: f.right  , edge15: f.left
       , edge26: f.right  , edge37: f.left }

-- TODO: bad poly
-- (Poly [(P2 40.0 72.0)
--       ,(P2 48.0 71.0)
--       ,(P2 56.0 69.0)
--       ,(P2 56.0 64.0)
--       ,(P2 40.0 48.0)
--       ,(P2 40.0 40.0)
--       ,(P2 48.0 40.0)
--       ,(P2 64.0 56.0)
--       ,(P2 69.0 56.0)
--       ,(P2 71.0 48.0)
--       ,(P2 72.0 40.0)
--       ,(P2 71.0 32.0)
--       ,(P2 69.0 24.0)
--       ,(P2 64.0 16.0)
--       ,(P2 56.0 11.0)
--       ,(P2 48.0 9.0)
--       ,(P2 40.0 8.0)
--       ,(P2 32.0 9.0)
--       ,(P2 24.0 11.0)
--       ,(P2 16.0 16.0)
--       ,(P2 11.0 24.0)
--       ,(P2 9.0 32.0)
--       ,(P2 8.0 40.0)
--       ,(P2 9.0 48.0)
--       ,(P2 11.0 56.0)
--       ,(P2 16.0 64.0)
--       ,(P2 24.0 69.0)
--       ,(P2 32.0 71.0)
--       ,(P2 40.0 72.0)
--       ,(P2 40.0 80.0)
--       ,(P2 32.0 79.0)
--       ,(P2 24.0 77.0)
--       ,(P2 16.0 72.0)
--       ,(P2 8.0 64.0)
--       ,(P2 3.0 56.0)
--       ,(P2 1.0 48.0)
--       ,(P2 0.0 40.0)
--       ,(P2 1.0 32.0)
--       ,(P2 3.0 24.0)
--       ,(P2 8.0 16.0)
--       ,(P2 16.0 8.0)
--       ,(P2 24.0 3.0)
--       ,(P2 32.0 1.0)
--       ,(P2 40.0 0.0)
--       ,(P2 48.0 1.0)
--       ,(P2 56.0 3.0)
--       ,(P2 64.0 8.0)
--       ,(P2 72.0 16.0)
--       ,(P2 77.0 24.0)
--       ,(P2 79.0 32.0)
--       ,(P2 80.0 40.0)
--       ,(P2 79.0 48.0)
--       ,(P2 77.0 56.0)
--       ,(P2 72.0 64.0)
--       ,(P2 64.0 72.0)
--       ,(P2 56.0 77.0)
--       ,(P2 48.0 79.0)
--       ,(P2 40.0 80.0)])
-- clip by ((48, 40), (56, 48)) square
