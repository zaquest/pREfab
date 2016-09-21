module Linear.R2 where

import Prelude
import Linear.Epsilon (class Epsilon, nearZero)
import Math (sqrt)

type R2Rec a = { x :: a, y :: a }

newtype V2 a = V2 (R2Rec a)
newtype P2 a = P2 (R2Rec a)

v2 :: forall a. a -> a -> V2 a
v2 x y = V2 {x: x, y: y} {-# INLINE v2 #-}

p2 :: forall a. a -> a -> P2 a
p2 x y = P2 {x: x, y: y} {-# INLINE p2 #-}

v2rec :: forall a. V2 a -> R2Rec a
v2rec (V2 rec) = rec

p2rec :: forall a. P2 a -> R2Rec a
p2rec (P2 rec) = rec

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

instance v2Epsilon :: Epsilon a => Epsilon (V2 a) where
    nearZero (V2 {x: x, y: y}) = nearZero x && nearZero y

instance p2Epsilon :: Epsilon a => Epsilon (P2 a) where
    nearZero (P2 {x: x, y: y}) = nearZero x && nearZero y

instance v2Show :: Show a => Show (V2 a) where
  show (V2 {x: x, y: y}) = "V2 " <> show x <> " " <> show y

instance p2Show :: Show a => Show (P2 a) where
  show (P2 {x: x, y: y}) = "P2 " <> show x <> " " <> show y

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

-- scalar-functor and functor-scalar ops
sAddF :: forall a f. (Semiring a, Functor f) => a -> f a -> f a
sAddF s f = add s <$> f
infixl 6 sAddF as +^

fAddS :: forall a f. (Semiring a, Functor f) => f a -> a -> f a
fAddS f s = flip add s <$> f
infixl 6 fAddS as ^+

sMulF :: forall a f. (Semiring a, Functor f) => a -> f a -> f a
sMulF s f = mul s <$> f
infixl 7 sMulF as *^

fMulS :: forall a f. (Semiring a, Functor f) => f a -> a -> f a
fMulS f s = flip mul s <$> f
infixl 7 fMulS as ^*

sSubF :: forall a f. (Ring a, Functor f) => a -> f a -> f a
sSubF s f = sub s <$> f
infixl 6 sSubF as -^

fSubS :: forall a f. (Ring a, Functor f) => f a -> a -> f a
fSubS f s = flip sub s <$> f
infixl 6 sSubF as ^-

sModF :: forall a f. (EuclideanRing a, Functor f) => a -> f a -> f a
sModF s f = mod s <$> f

fModS :: forall a f. (EuclideanRing a, Functor f) => f a -> a -> f a
fModS f s = flip mod s <$> f

sDivF :: forall a f. (EuclideanRing a, Functor f) => a -> f a -> f a
sDivF s f = div s <$> f
infixl 7 sDivF as /^

fDivS :: forall a f. (EuclideanRing a, Functor f) => f a -> a -> f a
fDivS f s = flip div s <$> f
infixl 7 fDivS as ^/

crossZ :: forall a. Ring a => V2 a -> V2 a -> a
crossZ (V2 {x: x1, y: y1}) (V2 {x: x2, y: y2}) = x1 * y2 - y1 * x2
{-# INLINE crossZ #-}

p2Diff :: forall a. Ring a => P2 a -> P2 a -> V2 a
p2Diff (P2 {x: x1, y: y1}) (P2 {x: x2, y: y2}) = V2 {x: x1 - x2, y: y1 - y2}
infixl 6 p2Diff as .-.

invY :: forall a. Ring a => P2 a -> P2 a
invY (P2 {x, y}) = P2 { x: x, y: (-y) }

norm :: V2 Number -> Number
norm (V2 {x, y}) = sqrt (x * x + y * y)

distance :: P2 Number -> P2 Number -> Number
distance a b = norm (a .-. b)

qd :: forall a. Semiring a => V2 a -> a
qd (V2 {x, y}) = x * x + y * y

quadrance :: forall a. Ring a => P2 a -> P2 a -> a
quadrance a b = qd (a .-. b)
