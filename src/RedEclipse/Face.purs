module RedEclipse.Face
     ( validatePoly
     , Validity
     , Plane(..)
     --, CFace
     --, gridPrev
     --, gridNext
     ) where

import Prelude
import Data.Int (ceil, round, toNumber)
import Data.Traversable (sequence, for)
import Data.Array ((..))
import Data.Polygon (Poly2, isConvex, length, origin, square, clip,
                     move, prunePoly)
import Data.BoundingBox (boundingBox)
import Linear.R2 (P2, p2, r2, V2(..))
import Linear.Vector ((^*))
import Linear.Epsilon ((~=))
import Data.Segment (Seg2)
import Data.Maybe (Maybe(..), maybe)
import RedEclipse.Face.Side (Side(..))
import Grid (CGrid, gridSize, gridPrev, gridNext)
import Data.Identity (Identity(..), runIdentity)

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
      validity = for (0..((height / gridSize) - 1)) $ \y ->
                   for (0..((width / gridSize) - 1)) $ \x ->
                     let cs = square (toNumber gridSize) (toNumber <$> p2 x y ^* gridSize)
                      in Identity (maybe true validFace (clip cs oPoly))
  in { lo: plo, hi: phi, validity: runIdentity validity }

-- toPlane :: Poly2 Int -> (P2 Int, Plane (Maybe Face))
-- toPlane aPoly = (,) lo $
--   V.generate (height `div` gridSize) $ \y ->
--     V.generate (width `div` gridSize) $ \x ->
--       let cs = square 8.0 (fromIntegral <$> P2 (x * gridSize) (y * gridSize))
--           intersection = clip cs oPoly
--           len = length (points intersection)
--       in if len < 3
--            then Just emptyFace
--            else fromPoly intersection
--   where
--     (lo, hi) = fmap gridPrev *** fmap gridNext $ boundingBox aPoly
--     hiFaceOrigin@(P2 width height) = hi - lo
--     oPoly :: Poly2 Float -- ^ aPoly moved to the I quarter
--     oPoly = (fmap fromIntegral . subtract lo) <$> aPoly

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
