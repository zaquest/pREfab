module Linear.Vector where

import Prelude
import Data.Foldable (class Foldable, foldl)

class Functor f <= Additive f where
  aZero :: forall a. Semiring a => f a
  vAddV :: forall a. Semiring a => f a -> f a -> f a
  vSubV :: forall a. Ring a => f a -> f a -> f a
  lerp :: forall a. Ring a => a -> f a -> f a -> f a
  liftU2 :: forall a. (a -> a -> a) -> f a -> f a -> f a
  liftI2 :: forall a b c. (a -> b -> c) -> f a -> f b -> f c

defaultZero :: forall f a. (Applicative f, Semiring a) => f a
defaultZero = pure zero
{-# INLINE defaultZero #-}

defaultVAddV :: forall f a. (Applicative f, Semiring a)
             => f a -> f a -> f a
defaultVAddV x y = defaultLiftU2 (+) x y
{-# INLINE defaultVAddV #-}

defaultVSubV :: forall f a. (Applicative f, Ring a)
             => f a -> f a -> f a
defaultVSubV x y = defaultLiftU2 (-) x y
{-# INLINE defaultVSubV #-}

defaultLerp :: forall f a. (Applicative f, Ring a)
            => a -> f a -> f a -> f a
defaultLerp alpha u v = defaultVAddV (alpha *^ u) ((one - alpha) *^ v)
{-# INLINE defaultLerp #-}

defaultLiftU2 :: forall f a. Applicative f
              => (a -> a -> a) -> f a -> f a -> f a
defaultLiftU2 = defaultLiftI2

defaultLiftI2 :: forall f a b c. Applicative f
              => (a -> b -> c) -> f a -> f b -> f c
defaultLiftI2 f x y = f <$> x <*> y
{-# INLINE defaultLiftI2 #-}

infixl 6 vAddV as ^+^
infixl 6 vSubV as ^-^

negated :: forall f a. (Functor f, Ring a) => f a -> f a
negated x = negate <$> x
{-# INLINE negated #-}

sumV :: forall f v a. (Foldable f, Additive v, Semiring a)
     => f (v a) -> v a
sumV = foldl (^+^) aZero
{-# INLINE sumV #-}

sMulF :: forall f a. (Functor f, Semiring a) => a -> f a -> f a
sMulF s f = mul s <$> f
{-# INLINE sMulF #-}
infixl 7 sMulF as *^

fMulS :: forall f a. (Functor f, Semiring a) => f a -> a -> f a
fMulS f s = flip mul s <$> f
{-# INLINE fMulS #-}
infixl 7 fMulS as ^*

sDivF :: forall f a. (Functor f, EuclideanRing a) => a -> f a -> f a
sDivF s f = div s <$> f
{-# INLINE sDivF #-}
infixl 7 sDivF as /^

fDivS :: forall f a. (Functor f, EuclideanRing a) => f a -> a -> f a
fDivS f s = flip div s <$> f
{-# INLINE fDivS #-}
infixl 7 fDivS as ^/

-- scalar-functor and functor-scalar ops
sAddF :: forall f a. (Functor f, Semiring a) => a -> f a -> f a
sAddF s f = add s <$> f
{-# INLINE sAddF #-}
infixl 6 sAddF as +^

fAddS :: forall f a. (Functor f, Semiring a) => f a -> a -> f a
fAddS f s = flip add s <$> f
{-# INLINE fAddS #-}
infixl 6 fAddS as ^+

sSubF :: forall f a. (Functor f, Ring a) => a -> f a -> f a
sSubF s f = sub s <$> f
{-# INLINE sSubF #-}
infixl 6 sSubF as -^

fSubS :: forall f a. (Functor f, Ring a) => f a -> a -> f a
fSubS f s = flip sub s <$> f
{-# INLINE fSubS #-}
infixl 6 sSubF as ^-

sModF :: forall a f. (EuclideanRing a, Functor f) => a -> f a -> f a
sModF s f = mod s <$> f
{-# INLINE sModF #-}
infixl 7 sModF as %^

fModS :: forall a f. (EuclideanRing a, Functor f) => f a -> a -> f a
fModS f s = flip mod s <$> f
{-# INLINE fModS #-}
infixl 7 fModS as ^%
