module Data.BoundingBox
     ( BoundingBox
     , bbRec
     , boundingBox
     , boundingRect
     , width
     , height
     ) where

import Prelude
import Linear.R2 (P2(..))
import Data.Polygon (Poly2, points, rect)
import Utils (foldlA2, fromJust)

type BoundingBox a = { lo :: P2 a, hi :: P2 a }

bbRec :: forall a. P2 a -> P2 a -> BoundingBox a
bbRec lo hi = {lo: lo, hi: hi}

boundingBox :: forall a. Ord a => Poly2 a -> BoundingBox a
boundingBox poly = fromJust $ bbRec <$> foldlA2 min ps
                                    <*> foldlA2 max ps
  where ps = points poly

boundingRect :: forall a. Ord a => Poly2 a -> Poly2 a
boundingRect poly = let bb = boundingBox poly
                     in rect bb.lo bb.hi

width :: forall a. Ring a => BoundingBox a -> a
width { lo: P2 lo, hi: P2 hi } = hi.x - lo.x

height :: forall a. Ring a => BoundingBox a -> a
height { lo: P2 lo, hi: P2 hi } = hi.y - lo.y
