module Data.Corner
     ( Corner(..)
     , Corner2
     , isLine
     , clockWise
     , crossZ
     , counterClockWise
     ) where

import Prelude
import Linear.R2 (P2, (.-.))
import Linear.R2 (crossZ) as R2
import Linear.Epsilon (class Epsilon, nearZero, (<<), (>>))

data Corner p = Corner p p p
type Corner2 a = Corner (P2 a)

-- Same as Data.Segment.onLine
isLine :: forall a. Ring a => Epsilon a => Corner2 a -> Boolean
isLine (Corner s e p) = nearZero (R2.crossZ (e .-. s) (p .-. e))

-- Same as Data.Segment.inside
clockWise :: forall a. Ring a => Epsilon a => Corner2 a -> Boolean
clockWise (Corner s e p) = R2.crossZ (e .-. s) (p .-. e) << zero

-- Same as Data.Segment.outside
counterClockWise :: forall a. Ring a => Epsilon a => Corner2 a -> Boolean
counterClockWise (Corner s e p) = R2.crossZ (e .-. s) (p .-. e) >> zero

crossZ :: forall a. Ring a => Epsilon a => Corner2 a -> a
crossZ (Corner s e p) = R2.crossZ (e .-. s) (p .-. e)
