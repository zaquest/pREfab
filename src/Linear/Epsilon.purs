module Linear.Epsilon where

import Prelude
--import Data.Ring (class Ring, (-))
--import Data.Eq (class Eq, (==))
--import Data.Ord (class Ord, compare, (<=), Ordering(..))
--import Data.BooleanAlgebra
import Math (abs)

class (Ring a, Ord a) <= Epsilon a where
    nearZero :: a -> Boolean

instance numberEpsilon :: Epsilon Number where
    nearZero x = abs x <= 1e-12

instance intEpsilon :: Epsilon Int where
    nearZero x = x == 0 {-# INLINE nearZero #-}

approxCompare :: forall a. Epsilon a => a -> a -> Ordering
approxCompare x y = if nearZero (x - y)
                       then EQ
                       else compare x y

approxEq :: forall a. Epsilon a => a -> a -> Boolean
approxEq x y = approxCompare x y == EQ
infix 4 approxEq as ~=

approxLessThanOrEq :: forall a. Epsilon a => a -> a -> Boolean
approxLessThanOrEq x y = let o = approxCompare x y in o == LT || o == EQ
infixl 4 approxLessThanOrEq as <~=

approxGreaterThanOrEq :: forall a. Epsilon a => a -> a -> Boolean
approxGreaterThanOrEq x y = let o = approxCompare x y in o == GT || o == EQ
infixl 4 approxGreaterThanOrEq as >~=

newtype Approx a = Approx a

toExact :: forall a. Approx a -> a
toExact (Approx a) = a

instance approxEqInstance :: Epsilon a => Eq (Approx a) where
    eq (Approx x) (Approx y) = approxEq x y

instance approxOrd :: Epsilon a => Ord (Approx a) where
    compare (Approx x) (Approx y) = approxCompare x y
