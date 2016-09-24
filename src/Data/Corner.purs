module Data.Corner
     ( Corner(..)
     , Corner2
     , isLine
     , clockWise
     , counterClockWise
     , crossZ
     ) where

import Prelude
import Linear.R2 (P2, (.-.))
import Linear.R2 (crossZ) as R2
import Linear.Epsilon (class Epsilon, nearZero, (<~=), (>~=))

data Corner p = Corner p p p
type Corner2 a = Corner (P2 a)

crossZ :: forall a. Ring a => Corner2 a -> a
crossZ (Corner x y z) = R2.crossZ (y .-. x) (z .-. y)
{-# INLINE crossZ #-}

isLine :: forall a. (Ring a, Epsilon a) => Corner2 a -> Boolean
isLine = nearZero <<< crossZ

clockWise :: forall a. (Ring a, Epsilon a) => Corner2 a -> Boolean
clockWise c = crossZ c >~= zero

counterClockWise :: forall a. (Ring a, Epsilon a) => Corner2 a -> Boolean
counterClockWise c = crossZ c <~= zero
