module Data.Polygon
     ( Polygon(..)
     , Poly2(..)
     , Repr
     , Line(..)
     , Line2(..)
     , Edge(..)
     , Edge2(..)
     , fromFoldable
     , points
     , simplify
     --, moveBy
     , boundingBox
     , showBBRec
     , origin
     , clip
     , isConvex
     , clockWise
     , length
     , uncons
     , square
     , sortBy
     , filter
     , null
     , elemIndex
     , at
     , update
     , insert
     , edges
     , onLine
     ) where

import Prelude
import Data.Foldable as F
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.List as L
import Control.Monad.Transformerless.State (modify, put, get, execState)

import Linear.Epsilon (class Epsilon, Approx(..), nearZero, toExact, (~=), (>~=), (<~=))
import Linear.R2 (P2(..), V2, (.+^), crossZ, (.-.), p2, (^+))

import Data.Traversable (for_)
import Utils (foldlA2)

type Repr = L.List

at :: forall a. Repr a -> Int -> Maybe a
at = L.index

length :: forall a. Repr a -> Int
length = L.length

cons :: forall a. a -> Repr a -> Repr a
cons = L.Cons

nil :: forall a. Repr a
nil = L.Nil

sortBy :: forall a. (a -> a -> Ordering) -> Repr a -> Repr a
sortBy = L.sortBy

filter :: forall a. (a -> Boolean) -> Repr a -> Repr a
filter = L.filter

emptyPoly :: forall p. Polygon p
emptyPoly = Poly nil

null :: forall a. Repr a -> Boolean
null = L.null

uncons :: forall a. Repr a -> Maybe { head :: a, tail :: Repr a }
uncons = L.uncons

zipWith :: forall a b c. (a -> b -> c) -> Repr a -> Repr b -> Repr c
zipWith = L.zipWith

zipWith3 :: forall a b c d. (a -> b -> c -> d) -> Repr a -> Repr b -> Repr c -> Repr d
zipWith3 f as bs = L.zipWith ($) (L.zipWith f as bs)

snoc :: forall a. Repr a -> a -> Repr a
snoc = L.snoc

splitAt :: forall a. Int -> Repr a -> { init :: Repr a, rest :: Repr a }
splitAt n xs = { init: L.take n xs, rest: L.drop n xs }

elemIndex :: forall a. Eq a => a -> Repr a -> Maybe Int
elemIndex = L.elemIndex

nub :: forall a. Eq a => Repr a -> Repr a
nub = L.nub

fromFoldable :: forall f p. F.Foldable f => f p -> Repr p
fromFoldable = L.fromFoldable

data Edge p = Edge p p
type Edge2 a = Edge (P2 a)

instance edgeFunctor :: Functor Edge where
  map f (Edge s e) = Edge (f s) (f e)

data Line p = Line p p
type Line2 a = Line (P2 a)

instance lineFunctor :: Functor Line where
  map f (Line s e) = Line (f s) (f e)

newtype Polygon p = Poly (Repr p)
type Poly2 a = Polygon (P2 a)

instance functorPoly :: Functor Polygon where
    map f (Poly ps) = Poly (map f ps)

instance showPoly :: Show p => Show (Polygon p) where
  show (Poly ps) = "(Poly " <> show ps <> ")"

-- fromPoint :: forall a. P2 a -> V2 a
-- fromPoint (P2 r2rec) = V2 r2rec

points :: forall p. Polygon p -> Repr p
points (Poly ps) = ps

edges :: forall p. Polygon p -> Repr (Edge p)
edges (Poly ps) = case uncons ps of
                    Nothing -> nil
                    Just {head: h, tail: t} -> zipWith Edge ps (snoc t h)

update :: forall p. Int -> p -> Polygon p -> Maybe (Polygon p)
update i p (Poly ps) = let ir = splitAt i ps
                       in do ht <- uncons ir.rest
                             pure <<< Poly $ ir.init <> (cons p ht.tail)

insert :: forall p. Int -> p -> Polygon p -> Polygon p
insert i p (Poly ps) = let ir = splitAt i ps
                       in Poly $ ir.init <> (cons p ir.rest)

type BBRec a = { lo :: P2 a, hi :: P2 a }

showBBRec :: forall a. Show a => BBRec a -> String
showBBRec {lo, hi} = "{ " <> show lo <> ", " <> show hi <> " }"

boundingBox :: forall a. Ord a => Poly2 a -> Maybe (BBRec a)
boundingBox (Poly ps) = bbRec <$> foldlA2 min ps <*> foldlA2 max ps
  where bbRec lo hi = { lo: lo, hi: hi }

boundingRect :: forall a. Ord a => Poly2 a -> Maybe (Poly2 a)
boundingRect poly = Poly <<< bbRecToRect <$> boundingBox poly
  where
    bbRecToRect :: { lo :: P2 a, hi :: P2 a } -> Repr (P2 a)
    bbRecToRect { lo: P2 lo
                , hi: P2 hi } = fromFoldable [ P2 lo
                                             , p2 lo.x hi.y
                                             , P2 hi
                                             , p2 hi.x lo.y ]

moveBy :: forall a. Semiring a => Poly2 a -> V2 a -> Poly2 a
moveBy poly t = (\p -> p .+^ t) <$> poly

origin :: forall a. Ord a => Poly2 a -> Maybe (P2 a)
origin (Poly ps) = foldlA2 min ps

-- | Nothing means that all points of the polygon lie on on line.
-- Only for convex polygons
clockWise :: forall a. (Ord a, Epsilon a) => Poly2 a -> Maybe Boolean
clockWise (Poly ps) = go ps
    where go ps = do
            ht1 <- uncons ps
            ht2 <- uncons ht1.tail
            ht3 <- uncons ht2.tail
            let z = pointCrossZ ht1.head ht2.head ht3.head
            if z ~= zero
               then go ht1.tail
               else Just (z < zero)

-- | https://en.wikipedia.org/wiki/Sutherland–Hodgman_algorithm
-- `clipPoly` have to be convex
clip :: Poly2 Number -> Poly2 Number -> Poly2 Number
clip clipPoly subjPoly = fromMaybe emptyPoly do
  isClockWise <- clockWise clipPoly
  let inside = if isClockWise then clock else counter
  pure $ sutherlandHodgman inside clipPoly subjPoly
  where
    --counter :: P2 Number -> Edge2 Number -> Boolean
    counter p (Edge cs ce) = let z = pointCrossZ cs ce p in z > 0.0 && not (nearZero z)
    --clock :: P2 Number -> Edge2 Number -> Boolean
    clock   p (Edge cs ce) = let z = pointCrossZ cs ce p in z < 0.0 && not (nearZero z)

-- | `inside p edge` point `p` is `inside` of `edge` if it lies on the
-- same side of `edge` as the reminder of the clip polygon
-- Complexity is supposed to be O(nm + k log(k)) where
-- n - number of points in clip polygon
-- m - number of points in subject ponlygon
-- k - number of points in intersection
-- k log(k) is to remove repeating points in resulting polygon
sutherlandHodgman :: (P2 Number -> Edge2 Number -> Boolean) -- ^ Is `P2 a` "inside" `Edge2 a` (on the same side as the rest of the `clipPoly`
                  -> Poly2 Number                           -- ^ Clip polygon (have to be convex)
                  -> Poly2 Number                           -- ^ Subject polygon
                  -> Poly2 Number                           -- ^ Intersection
sutherlandHodgman inside clipPoly subjPoly =
  Poly <<< nub' $ flip execState (points subjPoly) $
    for_ (edges clipPoly) $ \(Edge cs ce) -> do
      let clipEdge = Edge cs ce
      newPoly <- Poly <$> get
      put nil
      for_ (edges newPoly) $ \(Edge s e) ->
        let subjEdge = Edge s e in
        if e `inside` clipEdge
          then do
            unless (s `inside` clipEdge) $
              modify $ maybe id add (intersection subjEdge clipEdge)
            modify (add e)
          else
            when (s `inside` clipEdge) $
              modify $ maybe id add (intersection subjEdge clipEdge)
  where add = flip snoc
        nub' = map toExact <<< nub <<< map Approx

pointCrossZ :: forall a. Ring a => P2 a -> P2 a -> P2 a -> a
pointCrossZ x y z = crossZ (y .-. x) (z .-. y)

onLine :: forall a. (Ring a, Epsilon a) => P2 a -> P2 a -> P2 a -> Boolean
onLine x y z  = nearZero (pointCrossZ x y z)

intersection :: forall a. (Ord a, Epsilon a, EuclideanRing a) => Edge2 a -> Edge2 a -> Maybe (P2 a)
intersection (Edge (P2 {x: x1, y: y1}) (P2 {x: x2, y: y2})) (Edge (P2 {x: x3, y: y3}) (P2 {x: x4, y: y4})) =
  let denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
  in if nearZero denom
       then Nothing
       else let x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / denom
                y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / denom
            in Just (p2 x y)

-- | Based on http://stackoverflow.com/a/1881201/1211428
isConvex :: forall a. (Epsilon a, Ord a) => Poly2 a -> Maybe Boolean
isConvex (Poly ps) = do
  ht1 <- uncons ps
  ht2 <- uncons ht1.tail
  let cs = zipWith3 pointCrossZ ps ht1.tail ht2.tail
  pure $ F.all (\x -> x >~= zero) cs || F.all (\x -> x <~= zero) cs

square :: forall a. Semiring a => a -> P2 a -> Poly2 a
square side lo@(P2 o) = Poly $ fromFoldable [lo, p2 o.x (o.y + side), lo ^+ side, p2 (o.x + side) o.y]

-- | Repr dependent
simplify :: forall a. (Ring a, Epsilon a) => Repr (P2 a) -> Repr (P2 a)
simplify ps
  | length ps < 3 = ps
  | otherwise = fromMaybe ps do
    ht <- uncons ps
    l <- L.last ht.tail
    let ps' = simplify' (l `cons` ps `snoc` ht.head)
    t <- L.tail ps'
    ps'' <- L.init t
    pure ps''
  where
    simplify' (L.Cons p0 ps0@(L.Cons p1 ps1@(L.Cons p2 _))) =
      if onLine p0 p1 p2
        then L.Cons p0 (simplify' ps1)
        else L.Cons p0 (simplify' ps0)
    simplify' rest = rest
