module Linear.R2 where

import Prelude
import Data.Monoid ((<>))
import Data.Foldable (class Foldable)
import Linear.Epsilon (class Epsilon, nearZero)
import Linear.Vector ( class Additive, defaultZero, defaultVAddV
                     , defaultVSubV, defaultLerp, defaultLiftU2
                     , defaultLiftI2 )
import Linear.Metric ( class Metric, defaultDot, defaultQuadrance
                     , defaultQd, defaultDistance, defaultNorm
                     , defaultSignorm )
import Data.Generic (class Generic)

type R2Rec a = { x :: a, y :: a }

newtype V2 a = V2 { x :: a, y :: a }
newtype P2 a = P2 { x :: a, y :: a }

v2 :: forall a. a -> a -> V2 a
v2 x y = V2 {x: x, y: y} {-# INLINE v2 #-}

p2 :: forall a. a -> a -> P2 a
p2 x y = P2 {x: x, y: y} {-# INLINE p2 #-}

derive instance genericV2 :: Generic a => Generic (V2 a)
derive instance genericP2 :: Generic a => Generic (P2 a)

class R2 t where
  r2 :: forall a. t a -> R2Rec a

instance v2r2 :: R2 V2 where
  r2 (V2 rec) = rec

instance p2r2 :: R2 P2 where
  r2 (P2 rec) = rec

instance v2Semiring :: Semiring a => Semiring (V2 a) where
    one = V2 {x: one, y: one}
    mul (V2 {x: x1, y: y1}) (V2 {x: x2, y: y2}) = V2 { x: x1 * x2, y: y1 * y2 }
    zero = V2 {x: zero, y: zero}
    add (V2 {x: x1, y: y1}) (V2 {x: x2, y: y2}) = V2 { x: x1 + x2, y: y1 + y2 }

instance p2Semiring :: Semiring a => Semiring (P2 a) where
    one = P2 {x: one, y: one}
    mul (P2 {x: x1, y: y1}) (P2 {x: x2, y: y2}) = P2 { x: x1 * x2, y: y1 * y2 }
    zero = P2 {x: zero, y: zero}
    add (P2 {x: x1, y: y1}) (P2 {x: x2, y: y2}) = P2 { x: x1 + x2, y: y1 + y2 }

instance v2Ring :: Ring a => Ring (V2 a) where
    sub (V2 {x: x1, y: y1}) (V2 {x: x2, y: y2}) = V2 {x: x1 - x2, y: y1 - y2}

instance p2Ring :: Ring a => Ring (P2 a) where
    sub (P2 {x: x1, y: y1}) (P2 {x: x2, y: y2}) = P2 {x: x1 - x2, y: y1 - y2}

instance v2CommutativeRing :: CommutativeRing a => CommutativeRing (V2 a)
instance p2CommutativeRing :: CommutativeRing a => CommutativeRing (P2 a)

instance v2EuclideanRing :: EuclideanRing a => EuclideanRing (V2 a) where
  mod (V2 {x: x1, y: y1}) (V2 {x: x2, y: y2}) = V2 {x: mod x1 x2, y: mod y1 y2}
  div (V2 {x: x1, y: y1}) (V2 {x: x2, y: y2}) = V2 {x: div x1 x2, y: div y1 y2}
  degree (V2 {x: x}) = degree x

instance p2EuclideanRing :: EuclideanRing a => EuclideanRing (P2 a) where
  mod (P2 {x: x1, y: y1}) (P2 {x: x2, y: y2}) = P2 {x: mod x1 x2, y: mod y1 y2}
  div (P2 {x: x1, y: y1}) (P2 {x: x2, y: y2}) = P2 {x: div x1 x2, y: div y1 y2}
  degree (P2 {x: x}) = degree x

instance v2Field :: Field a => Field (V2 a)
instance p2Field :: Field a => Field (P2 a)

instance v2Eq :: Eq a => Eq (V2 a) where
  eq (V2 {x: x1, y: y1}) (V2 {x: x2, y: y2}) = x1 == x2 && y1 == y2

instance p2Eq :: Eq a => Eq (P2 a) where
  eq (P2 {x: x1, y: y1}) (P2 {x: x2, y: y2}) = x1 == x2 && y1 == y2

instance v2Ord :: Ord a => Ord (V2 a) where
  compare (V2 {x: x1, y: y1}) (V2 {x: x2, y: y2}) =
    let cmp = compare x1 x2
    in case cmp of
            EQ -> compare y1 y2
            _  -> cmp

instance p2Ord :: Ord a => Ord (P2 a) where
  compare (P2 {x: x1, y: y1}) (P2 {x: x2, y: y2}) =
    let cmp = compare x1 x2
    in case cmp of
         EQ -> compare y1 y2
         _  -> cmp

instance v2Functor :: Functor V2 where
  map f (V2 {x: x, y: y}) = V2 {x: f x, y: f y}

instance p2Functor :: Functor P2 where
  map f (P2 {x: x, y: y}) = P2 {x: f x, y: f y}

instance v2Apply :: Apply V2 where
  apply (V2 {x: fx, y: fy}) (V2 {x: x, y: y}) = v2 (fx x) (fy y)

instance p2Apply :: Apply P2 where
  apply (P2 {x: fx, y: fy}) (P2 {x: x, y: y}) = p2 (fx x) (fy y)

instance v2Applicative :: Applicative V2 where
  pure x = v2 x x

instance p2Applicative :: Applicative P2 where
  pure x = p2 x x

instance v2Additive :: Additive V2 where
  aZero = defaultZero
  vAddV = defaultVAddV
  vSubV = defaultVSubV
  lerp = defaultLerp
  liftU2 = defaultLiftU2
  liftI2 = defaultLiftI2

instance p2Additive :: Additive P2 where
  aZero = defaultZero
  vAddV = defaultVAddV
  vSubV = defaultVSubV
  lerp = defaultLerp
  liftU2 = defaultLiftU2
  liftI2 = defaultLiftI2

instance v2Metric :: Metric V2 where
  dot = defaultDot
  quadrance = defaultQuadrance
  qd = defaultQd
  distance = defaultDistance
  norm = defaultNorm
  signorm = defaultSignorm

instance p2Metric :: Metric P2 where
  dot = defaultDot
  quadrance = defaultQuadrance
  qd = defaultQd
  distance = defaultDistance
  norm = defaultNorm
  signorm = defaultSignorm

instance v2Epsilon :: Epsilon a => Epsilon (V2 a) where
    nearZero (V2 {x: x, y: y}) = nearZero x && nearZero y

instance p2Epsilon :: Epsilon a => Epsilon (P2 a) where
    nearZero (P2 {x: x, y: y}) = nearZero x && nearZero y

instance v2Foldable :: Foldable V2 where
  foldMap f (V2 {x, y}) = f x <> f y
  foldl f acc (V2 {x, y}) = f (f acc x) y
  foldr f acc (V2 {x, y}) = f x (f y acc)

instance p2Foldable :: Foldable P2 where
  foldMap f (P2 {x, y}) = f x <> f y
  foldl f acc (P2 {x, y}) = f (f acc x) y
  foldr f acc (P2 {x, y}) = f x (f y acc)

instance v2Show :: Show a => Show (V2 a) where
  show (V2 {x: x, y: y}) = "(V2 " <> show x <> " " <> show y <> ")"

instance p2Show :: Show a => Show (P2 a) where
  show (P2 {x: x, y: y}) = "(P2 " <> show x <> " " <> show y <> ")"

-- point-vector ops
pAddV :: forall a. Semiring a => P2 a -> V2 a -> P2 a
pAddV (P2 {x: px, y: py}) (V2 {x: vx, y: vy}) = P2 {x: add px vx, y: add py vy}
infixl 6 pAddV as .+^

pMulV :: forall a. Semiring a => P2 a -> V2 a -> P2 a
pMulV (P2 {x: px, y: py}) (V2 {x: vx, y: vy}) = P2 {x: mul px vx, y: mul py vy}
infixl 7 pMulV as .*^

pSubV :: forall a. Ring a => P2 a -> V2 a -> P2 a
pSubV (P2 {x: px, y: py}) (V2 {x: vx, y: vy}) = P2 {x: sub px vx, y: sub py vy}
infixl 6 pSubV as .-^

pModV :: forall a. EuclideanRing a => P2 a -> V2 a -> P2 a
pModV (P2 {x: px, y: py}) (V2 {x: vx, y: vy}) = P2 {x: mod px vx, y: mod py vy}

pDivV :: forall a. EuclideanRing a => P2 a -> V2 a -> P2 a
pDivV (P2 {x: px, y: py}) (V2 {x: vx, y: vy}) = P2 {x: div px vx, y: div py vy}
infixl 7 pDivV as ./^

crossZ :: forall a. Ring a => V2 a -> V2 a -> a
crossZ (V2 {x: x1, y: y1}) (V2 {x: x2, y: y2}) = x1 * y2 - y1 * x2
{-# INLINE crossZ #-}

p2Diff :: forall a. Ring a => P2 a -> P2 a -> V2 a
p2Diff (P2 {x: x1, y: y1}) (P2 {x: x2, y: y2}) = V2 {x: x1 - x2, y: y1 - y2}
infixl 6 p2Diff as .-.

invY :: forall a. Ring a => P2 a -> P2 a
invY (P2 {x, y}) = P2 { x: x, y: (-y) }
