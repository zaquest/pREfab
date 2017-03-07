module Data.Polygon
     ( Poly
     , Poly2
     , mkPoly
     , uncons
     , updateAt
     , insertAt
     , index
     , length
     , points

     , edges
     , corners

     , origin
     , rect
     , square
     , move
     , clockWise
     , sutherlandHodgman
     , clip
     , isConvex
     , prunePoly
     , containsPoint

     , defaultPoly
     ) where

import Prelude
import Data.Int (odd)
import Data.Maybe (Maybe(..), maybe)
import Data.Array ( length, uncons, snoc, zipWith, nub, updateAt
                  , insertAt, fromFoldable, take, drop, unsafeIndex
                  , singleton ) as A
import Partial.Unsafe (unsafePartial)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, all, sum)
import Data.Traversable (class Traversable, sequence, traverse, for_)
import Control.Monad.State ( modify, put, get, execState )
import Data.List (List(..), fromFoldable, tail) as L
import Data.Enum (fromEnum)

import Linear.Epsilon (class Epsilon, toExact, Approx(..), (<~=),
                       (>~=))
import Linear.R2 (P2(..), V2, (.+^), p2)
import Data.Segment (Seg(..), Seg2, intersection, end)
import Data.Segment (inside, outside) as S
import Data.Corner (Corner(..), isLine)
import Data.Corner (clockWise, crossZ) as C
import Utils (bug, quot, foldlA2, fromJust, zipWith3)
import Data.Generic (class Generic)
import Grid (CGrid)

newtype Poly p = Poly (Array p)
type Poly2 a = Poly (P2 a)

mkPoly :: forall p. Array p -> Maybe (Poly p)
mkPoly ps | A.length ps < 3 = Nothing
          | otherwise = Just (Poly ps)

points :: forall p. Poly p -> Array p
points (Poly ps) = ps

edges :: forall p. Poly p -> Array (Seg p)
edges (Poly ps) = case A.uncons ps of
                    Nothing -> bug "edges: got empty polygon"
                    Just {head, tail} ->
                      A.zipWith Seg ps (tail `A.snoc` head)

rot :: forall a. Int -> Array a -> Array a
rot n arr = A.drop n' arr <> A.take n' arr
  where n' = n `quot` (A.length arr)

corners :: forall p. Poly p -> Array (Corner p)
corners (Poly ps0) = case mcs of
                       Nothing -> bug "corners: got bad polygon"
                       Just cs -> cs
  where mcs = do
          {head: h0, tail: t0} <- A.uncons ps0
          let ps1 = t0 `A.snoc` h0
          {head: h1, tail: t1} <- A.uncons ps1
          let ps2 = t1 `A.snoc` h1
          pure $ zipWith3 Corner ps0 ps1 ps2

-- | Any index is accepted, because polygon is treated as a cycle
index :: forall p. Poly p -> Int -> p
index (Poly ps) n = unsafePartial $
                      A.unsafeIndex ps (n `quot` (A.length ps))

length :: forall p. Poly p -> Int
length (Poly ps) = A.length ps

-- waiting for 0.10.1
--derive newtype instance polyFunctor :: Functor Poly
--derive newtype instance polyFoldable :: Foldable Poly
--derive newtype instance polyTraversable :: Traversable Poly

derive instance genericPoly :: Generic p => Generic (Poly p)

instance eqPoly :: Eq p => Eq (Poly p) where
  eq (Poly p1) (Poly p2) = p1 == p2

instance polyFunctor :: Functor Poly where
  map f (Poly ps) = Poly (f <$> ps)

instance polyFoldable :: Foldable Poly where
  foldMap f (Poly ps) = foldMap f ps
  foldl f acc (Poly ps) = foldl f acc ps
  foldr f acc (Poly ps) = foldr f acc ps

instance polyTraversable :: Traversable Poly where
  sequence (Poly ps) = Poly <$> sequence ps
  traverse f (Poly ps) = Poly <$> traverse f ps

instance showPoly :: Show p => Show (Poly p) where
  show (Poly ps) = "(Poly " <> show ps <> ")"

rect :: forall a. P2 a -> P2 a -> Poly2 a
rect (P2 lo) (P2 hi) = Poly [ P2 lo
                            , p2 lo.x hi.y
                            , P2 hi
                            , p2 hi.x lo.y ]

square :: forall a. Semiring a => a -> P2 a -> Poly2 a
square side lo = rect lo (lo + p2 side side)

origin :: forall a. Ord a => Poly2 a -> P2 a
origin (Poly ps) = fromJust $ foldlA2 min ps

-- | Only for convex polygons
clockWise :: forall a. (Ring a, Epsilon a) => Poly2 a -> Boolean
clockWise poly = all C.clockWise (corners poly)

-- | Type of a function that checks if a point lies inside of an edge
type InsideFn = P2 Number -> Seg2 Number -> Boolean

-- | `inside p edge` point `p` is `inside` of `edge` if it lies on the
-- same side of `edge` as the reminder of the clip polygon
-- Complexity is supposed to be O(nm + k log(k)) where
-- n - number of points in clip polygon
-- m - number of points in subject ponlygon
-- k - number of points in intersection
-- k log(k) is to remove repeating points in resulting polygon
sutherlandHodgman ::
     -- | Is `P2 Number` "inside" `Seg2 Number` (on the same side as
     -- the rest of the `clipPoly`
     (P2 Number -> Seg2 Number -> Boolean)
  -> Poly2 Number  -- ^ Clip polygon (have to be convex)
  -> Poly2 Number  -- ^ Subject polygon
  -> Maybe (Poly2 Number)  -- ^ Intersection
sutherlandHodgman inside (Poly clipPoly) (Poly subjPoly) =
  mkPoly <<< nub' $ flip execState subjPoly $
    for_ (ptEdges clipPoly) $ \(Seg cs ce) -> do
      let clipEdge = Seg cs ce
      newPoly <- get
      put []
      for_ (ptEdges newPoly) $ \(Seg s e) ->
        let subjEdge = Seg s e in
        if e `inside` clipEdge
           then do
             unless (s `inside` clipEdge) $
               modify $ maybe id add (intersection subjEdge clipEdge)
             modify (add e)
           else
             when (s `inside` clipEdge) $
               modify $ maybe id add (intersection subjEdge clipEdge)
  where add = flip A.snoc
        nub' = map toExact <<< A.nub <<< map Approx
        ptEdges ps = A.zipWith Seg ps (rot 1 ps)


-- | Step in Sutherland-Hodgman algorithm that clips off the part of
-- a subject polygon that is outside of a one clip polygon edge.
clipEdgePoly :: InsideFn -> Seg2 Number -> Poly2 Number -> Array (P2 Number)
clipEdgePoly inside edge@(Seg cs ce) poly = []

-- | Step in Sutherland-Hodgman algorithm that clips off the part of
-- a subject edge that is outside of a clip polygon edge.
clipEdgeEdge :: InsideFn -> Seg2 Number -> Seg2 Number -> Array (P2 Number)
clipEdgeEdge inside clipEdge subjEdge =
  if end isInside
     then []  <> [end subjEdge]
     else []
  where isInside = (_ `inside` clipEdge) <$> subjEdge
        intersect a = maybe [] A.singleton (intersection subjEdge clipEdge)


-- | https://en.wikipedia.org/wiki/Sutherland–Hodgman_algorithm
-- `clipPoly` have to be convex
clip :: Poly2 Number -> Poly2 Number -> Maybe (Poly2 Number)
clip clipPoly subjPoly =
  let inside = if clockWise clipPoly then S.inside else S.outside
   in sutherlandHodgman inside clipPoly subjPoly

-- | Based on http://stackoverflow.com/a/1881201/1211428
isConvex :: forall a. (Ring a, Epsilon a) => Poly2 a -> Boolean
isConvex poly =
  let czs = C.crossZ <$> corners poly
   in all (\z -> z >~= zero) czs || all (\z -> z <~= zero) czs

type Chain p = L.List p
type Chain2 a = Chain (P2 a)

-- | Takes a polygonal chain and removes middle points of segments
pruneChain :: forall a. (Ring a, Epsilon a) => Chain2 a -> Chain2 a
pruneChain (L.Cons p0 ps0@(L.Cons p1 ps1@(L.Cons p2 _))) =
  if isLine (Corner p0 p1 p2)
     then pruneChain (L.Cons p0 ps1)
     else L.Cons p0 (pruneChain ps0)
pruneChain rest = rest

polyToChain :: forall p. Poly p -> Chain p
polyToChain (Poly ps) = L.fromFoldable ps

chainToPoly :: forall p. Chain p -> Maybe (Poly p)
chainToPoly ps = mkPoly (A.fromFoldable ps)

prunePoly :: forall a. (Ring a, Epsilon a)
          => Poly2 a -> Maybe (Poly2 a)
prunePoly poly =
  let chain = L.Cons (poly `index` (-1)) (polyToChain poly)
      chain' = pruneChain chain
   in L.tail chain' >>= chainToPoly

move :: forall a. Semiring a => Poly2 a -> V2 a -> Poly2 a
move poly v = (\p -> p .+^ v) <$> poly

crossingNumber :: forall a. (Ord a, EuclideanRing a)
               => Poly2 a -> P2 a -> Int
crossingNumber poly (P2 p) = sum (map crossCnt $ edges poly)
  where
    crossCnt (Seg (P2 s) (P2 e)) =
      let intersectx = s.x + (p.y - s.y) * (e.x - s.x) / (e.y - s.y)
          upwardCross   = s.y <= p.y && p.y < e.y
          downwardCross = e.y <= p.y && p.y < s.y
       in fromEnum $ (upwardCross || downwardCross) && (p.x < intersectx)

-- | Crossing number test for a point in a polygon.
containsPoint :: forall a. (Ord a, EuclideanRing a)
              => Poly2 a -> P2 a -> Boolean
containsPoint poly p = odd (crossingNumber poly p)

updateAt :: forall p. Int -> p -> Poly p -> Poly p
updateAt idx p (Poly ps) = Poly (fromJust (A.updateAt idx' p ps))
  where idx' = idx `quot` (A.length ps)

insertAt :: forall p. Int -> p -> Poly p -> Poly p
insertAt idx p (Poly ps) = Poly (fromJust (A.insertAt idx' p ps))
  where idx' = idx `quot` (A.length ps)

uncons :: forall p. Poly p -> {head :: p, tail :: Array p}
uncons (Poly ps) = fromJust (A.uncons ps)

defaultPoly :: Poly2 CGrid
defaultPoly = fromJust $ mkPoly [p2 0 2, p2 16 0, p2 8 8, p2 (-1) 16]
