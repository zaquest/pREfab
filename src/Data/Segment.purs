module Data.Segment
     ( Seg(..)
     , Seg2
     , intersection
     , inside
     , outside
     , onLine
     , crossZ
     , nearestPoint
     , distance
     ) where

import Prelude
import Data.Maybe (Maybe(..))
import Linear.R2 (P2(..), p2, (.-.), (.+^))
import Linear.R2 (crossZ) as R2
import Linear.Epsilon (class Epsilon, nearZero, (<<), (>>))
import Linear.Vector ((*^))
import Linear.Metric (qd, dot)
import Linear.Metric (distance) as LM

-- | Represents segment in ends of type `p`
data Seg p = Seg p p

-- | Represents segment in R2
type Seg2 a = Seg (P2 a)

-- | Tries to find intersection point of two lines given by segments.
intersection :: forall a. (Epsilon a, EuclideanRing a)
             => Seg2 a -> Seg2 a -> Maybe (P2 a)
intersection (Seg (P2 {x: x1, y: y1}) (P2 {x: x2, y: y2}))
             (Seg (P2 {x: x3, y: y3}) (P2 {x: x4, y: y4})) =
  let denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
   in if nearZero denom
         then Nothing
         else let c12 = x1 * y2 - y1 * x2
                  c34 = x3 * y4 - y3 * x4
                  x = (c12 * (x3 - x4) - (x1 - x2) * c34) / denom
                  y = (c12 * (y3 - y4) - (y1 - y2) * c34) / denom
               in Just (p2 x y)

-- | Counter clock-wise
-- Same as Data.Corner.counterClockWise
outside :: forall a. (Ring a, Epsilon a) => P2 a -> Seg2 a -> Boolean
outside p seg = crossZ p seg >> zero

-- | Clock-wise
-- Same as Data.Corner.clockWise
inside :: forall a. (Ring a, Epsilon a) => P2 a -> Seg2 a -> Boolean
inside p seg = crossZ p seg << zero

-- | Checks if point `p` lies on the same line as segment `seg`
-- Same as Data.Corner.isLine
onLine :: forall a. (Ring a, Epsilon a) => P2 a -> Seg2 a -> Boolean
onLine p seg = nearZero (crossZ p seg)

-- | Corner s e p
crossZ :: forall a. (Ring a, Epsilon a) => P2 a -> Seg2 a -> a
crossZ p (Seg s e) = R2.crossZ (e .-. s) (p .-. e)
{-# INLINE crossZ #-}

-- | Returns nearest `ab` segment point to point `p`
-- http://stackoverflow.com/a/1501725
nearestPoint :: P2 Number -> Seg2 Number -> P2 Number
nearestPoint p (Seg a b) = if nearZero len2 then a else proj
  where len2 = qd a b
        -- Consider the line extending the segment,
        -- parameterized as a + t (b - a). We find projection
        -- of point p onto the line. It falls where
        -- t = [(p-a) . (b-a)] / |b-a|^2
        -- We clamp t from [0,1] to handle points outside the
        -- segment ab.
        t = max 0.0 (min 1.0 ((p - a) `dot` (b - a) / len2))
        proj = a .+^ t *^ (b .-. a)

-- | Minimal distance between point `p` and segment `ab`.
distance :: P2 Number -> Seg2 Number -> Number
distance p ab = LM.distance p (nearestPoint p ab)
