module RedEclipse.Face
     ( validatePoly
     , ValidityRec
     , Plane(..)
     --, CFace
     --, gridPrev
     --, gridNext
     ) where

import Prelude
import Data.Int (ceil, round, toNumber)
import Data.Traversable (sequence, for)
import Data.Array ((..))
import Data.Polygon (Polygon(..), Poly2, Line2, isConvex, length, origin, boundingBox, square, clip, points)
import Linear.R2 (P2, p2, p2rec, (^*))
import Linear.Epsilon ((~=))
import Data.Maybe (Maybe(..))
import RedEclipse.Face.Side (Side(..))
import Grid (CGrid, gridSize, gridPrev, gridNext)
import Data.Identity (Identity(..), runIdentity)

-- | Face coordinate type. In range [0, 8]
type CFace = Int

type Corner = P2 Side

allCorners :: Array Corner
allCorners = [p2 SDown SLeft, p2 SUp SLeft, p2 SUp SRight, p2 SDown SRight]

type ASide a = { side :: Side, edge :: Line2 a }

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

validFace :: Poly2 Number -> Boolean
validFace poly@(Poly ps) = has3Or4Edges && onGridAndConvex
  where
    len = length ps
    has3Or4Edges = len == 3 || len == 4
    onGridAndConvex = Just true == do
      o <- map (gridPrev <<< ceil) <$> origin poly
      ps' <- sequence $ toFaceCoords o <$> ps
      isConvex (Poly ps')

type Plane a = Array (Array a)

type ValidityRec = { lo :: P2 CGrid, hi :: P2 CGrid, validity :: Plane Boolean }

validatePoly :: Poly2 CGrid -> Maybe ValidityRec
validatePoly poly@(Poly ps) = do
  bb <- (\b -> { lo: gridPrev <$> b.lo, hi: gridNext <$> b.hi }) <$> boundingBox poly
  let poly' = (\p -> p - bb.lo) <$> poly
  let width = (p2rec bb.hi).x - (p2rec bb.lo).x
  let height = (p2rec bb.hi).y - (p2rec bb.lo).y
  let oPoly = map toNumber <$> poly'
  let validity = for (0..((height / gridSize) - 1)) $ \y ->
                   for (0..((width / gridSize) - 1)) $ \x ->
                     let cs = square (toNumber gridSize) (toNumber <$> p2 x y ^* gridSize)
                         intersection = clip cs oPoly
                     in Identity (length (points intersection) < 3 || validFace intersection)
  pure { lo: bb.lo, hi: bb.hi, validity: runIdentity validity }

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
toFaceCoords o p = if onGrid && inFace (p2rec f).x && inFace (p2rec f).y
                     then Just f
                     else Nothing
  where
    p' = round <$> p
    f = p' - o
    onGrid = p ~= (toNumber <$> p')
    inFace z = 0 <= z && z <= gridSize
