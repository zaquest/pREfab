-- vim: cc=70
module Linear.Metric where

import Prelude
import Math (sqrt)
import Data.Foldable (class Foldable, sum)
import Linear.Epsilon (nearZero, (~=))
import Linear.Vector (class Additive, (^-^), (^/), (*^), liftI2)

class Additive f <= Metric f where
  dot :: forall a. Semiring a => f a -> f a -> a
  quadrance :: forall a. Semiring a => f a -> a
  qd :: forall a. Ring a => f a -> f a -> a
  distance :: f Number -> f Number -> Number
  norm :: f Number -> Number
  signorm :: f Number -> f Number

defaultDot :: forall f a. Additive f => Foldable f => Semiring a
           => f a -> f a -> a
defaultDot x y = sum $ liftI2 (*) x y

defaultQuadrance :: forall f a. Additive f => Foldable f => Semiring a
                 => f a -> a
defaultQuadrance v = defaultDot v v

defaultQd :: forall f a. Additive f => Foldable f => Ring a
          => f a -> f a -> a
defaultQd x y = defaultQuadrance (x ^-^ y)

defaultDistance :: forall f. Additive f => Foldable f
                => f Number -> f Number -> Number
defaultDistance x y = defaultNorm (x ^-^ y)

defaultNorm :: forall f. Additive f => Foldable f
            => f Number -> Number
defaultNorm v = sqrt (defaultQuadrance v)

defaultSignorm :: forall f. Additive f => Foldable f
               => f Number -> f Number
defaultSignorm v = v ^/ (defaultNorm v)

normalize :: forall f. Metric f => f Number -> f Number
normalize v = if nearZero l || l ~= 1.0 then v else v ^/ (sqrt l)
  where l = quadrance v

-- | @project u v@ computes projection of `v` onto `u`
project :: forall f. Metric f => f Number -> f Number -> f Number
project u v = ((v `dot` u) / quadrance u) *^ u
