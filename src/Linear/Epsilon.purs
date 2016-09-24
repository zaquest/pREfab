module Linear.Epsilon where

import Data.Ring (class Ring, (-))
import Data.Eq (class Eq, (==))
import Data.Ord (class Ord, compare, (<=), Ordering(..))
import Data.BooleanAlgebra ((||))
import Math (abs)

class Ord a <= Epsilon a where
    nearZero :: a -> Boolean

instance numberEpsilon :: Epsilon Number where
    nearZero x = abs x <= 1e-12 {-# INLINE nearZero #-}

instance intEpsilon :: Epsilon Int where
    nearZero x = x == 0 {-# INLINE nearZero #-}

approxCompare :: forall a. (Ring a, Epsilon a) => a -> a -> Ordering
approxCompare x y = if nearZero (x - y)
                       then EQ
                       else compare x y

approxEQ :: forall a. (Ring a, Epsilon a) => a -> a -> Boolean
approxEQ x y = approxCompare x y == EQ
infix 4 approxEQ as ~=

approxLessThanOrEq :: forall a. (Ring a, Epsilon a) => a -> a -> Boolean
approxLessThanOrEq x y = let o = approxCompare x y in o == LT || o == EQ
infixl 4 approxLessThanOrEq as <~=

approxLT :: forall a. (Ring a, Epsilon a) => a -> a -> Boolean
approxLT x y = approxCompare x y == LT
infixl 4 approxLT as <<

approxGT :: forall a. (Ring a, Epsilon a) => a -> a -> Boolean
approxGT x y = approxCompare x y == GT
infixl 4 approxGT as >>

approxGreaterThanOrEq :: forall a. (Ring a, Epsilon a) => a -> a -> Boolean
approxGreaterThanOrEq x y = let o = approxCompare x y in o == GT || o == EQ
infixl 4 approxGreaterThanOrEq as >~=

newtype Approx a = Approx a

toExact :: forall a. Approx a -> a
toExact (Approx a) = a

instance approxEqInstance :: (Ring a, Epsilon a) => Eq (Approx a) where
    eq (Approx x) (Approx y) = approxEQ x y

instance approxOrd :: (Ring a, Epsilon a) => Ord (Approx a) where
    compare (Approx x) (Approx y) = approxCompare x y
