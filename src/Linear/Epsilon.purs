module Linear.Epsilon
  ( class Epsilon
  , nearZero
  , approxCompare
  , approxEQ
  , (~=)
  , approxLT
  , (<<)
  , approxGT
  , (>>)
  , approxLessThanOrEq
  , (<~=)
  , approxGreaterThanOrEq
  , (>~=)
  , Approx(..)
  , toExact
  ) where

import Data.Ring (class Ring, (-))
import Data.Eq (class Eq, (==))
import Data.Ord (class Ord, compare, (<=), Ordering(..))
import Data.BooleanAlgebra ((||))
import Math (abs)

-- | A type class for types that can be approximately equal zero.
-- |
-- | * `nearZero val` returns `true` if `val` is close enough to zero
-- |   for a particular type.
class Ord a <= Epsilon a where
    nearZero :: a -> Boolean

instance numberEpsilon :: Epsilon Number where
    nearZero x = abs x <= 1e-12 {-# INLINE nearZero #-}

instance intEpsilon :: Epsilon Int where
    nearZero x = x == 0 {-# INLINE nearZero #-}

-- | Compare two numbers. Consider them equal if their difference is
-- | approximately zero as given by `nearZero` from `Epsilon` type
-- | class isntance.
approxCompare :: forall a. Ring a => Epsilon a => a -> a -> Ordering
approxCompare x y = if nearZero (x - y)
                       then EQ
                       else compare x y

-- | `approxEQ` / `(~=)` is an approximate version of `eq` / `(==)`.
approxEQ :: forall a. Ring a => Epsilon a => a -> a -> Boolean
approxEQ x y = approxCompare x y == EQ
infix 4 approxEQ as ~=

-- | `approxLT` / `(<<)` is an approximate version of
-- | `lessThan` / `(<)`.
approxLT :: forall a. Ring a => Epsilon a => a -> a -> Boolean
approxLT x y = approxCompare x y == LT
infixl 4 approxLT as <<

-- | `approxGT` / `(>>)` is an approximate version of
-- | `greaterThan` / `(>)`.
approxGT :: forall a. Ring a => Epsilon a => a -> a -> Boolean
approxGT x y = approxCompare x y == GT
infixl 4 approxGT as >>

-- | `approxLessThanOrEq` / `(<~=)` is an approximate version of
-- | `lessThanOrEq` / `(<=)`.
approxLessThanOrEq :: forall a. Ring a => Epsilon a => a -> a -> Boolean
approxLessThanOrEq x y = let o = approxCompare x y
                          in o == LT || o == EQ
infixl 4 approxLessThanOrEq as <~=

-- | `approxGreaterThanOrEq` / `(>~=)` is an approximate version of
-- | `greaterThanOrEq` / `(>=)`.
approxGreaterThanOrEq :: forall a. Ring a => Epsilon a => a -> a -> Boolean
approxGreaterThanOrEq x y = let o = approxCompare x y
                             in o == GT || o == EQ
infixl 4 approxGreaterThanOrEq as >~=

-- | A wrapper type that has approximate `Eq` and `Ord` instances.
-- | Useful when you want to use a function that relies on `Eq` or
-- | `Ord` instance instead of writing your own that uses `Epsilon`.
newtype Approx a = Approx a

-- | Unwrap `Approx` value.
toExact :: forall a. Approx a -> a
toExact (Approx a) = a

instance approxEqInstance :: (Ring a, Epsilon a) => Eq (Approx a) where
    eq (Approx x) (Approx y) = approxEQ x y

instance approxOrd :: (Ring a, Epsilon a) => Ord (Approx a) where
    compare (Approx x) (Approx y) = approxCompare x y
