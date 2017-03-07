module Utils
     ( enumerate
     , foldlA2
     , whileM_
     , bug
     , quot
     , fromJust
     , zipWith3
     , replicateA_
     ) where

import Prelude
import Data.Traversable (class Traversable, for, traverse_)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.Maybe (fromJust) as M
import Control.Monad.State (evalState, put, get)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Data.Array (zipWith) as A
import Data.Array ((..))

-- | Applicative specialized `foldl1`.
foldlA2 :: forall t f a. (Foldable t, Applicative f)
        => (a -> a -> a) -> t (f a) -> Maybe (f a)
foldlA2 f = foldl af Nothing
  where af Nothing a = Just a
        af (Just b) a = Just (f <$> b <*> a)

-- | Monadic while loop.
whileM_ :: forall a m. Monad m
        => a -> (a -> Boolean) -> (a -> m a) -> m Unit
whileM_ start test body | test start = do next <- body start
                                          whileM_ next test body
                        | otherwise = pure unit

-- | Enumerate elements of traversal container.
enumerate :: forall a t. Traversable t
          => t a -> t { idx :: Int, elem :: a}
enumerate xs = flip evalState 0 <<< for xs $ \x -> do
                 idx <- get
                 put (idx + 1)
                 pure { idx: idx, elem: x }

-- | `bug errMsg` crashes a program with an error message `errMsg`
bug :: forall a. String -> a
bug = unsafeCrashWith

-- Like @x `mod` y@ but always returns value in range @[0, y)@, even
-- if `x` is negative.
quot :: Int -> Int -> Int
quot x y = let z = x `mod` y
           in if z < 0
                then z + y
                else z

-- | Unsafe `fromJust` for when you know for sure that you have
-- | `Just`.
fromJust :: forall a. Maybe a -> a
fromJust m = unsafePartial (M.fromJust m)

-- | `zipWith3 f as bs cs` zips three arrays `as`, `bs` and `cs` using
-- | a function `f`.
zipWith3 :: forall a b c d
          . (a -> b -> c -> d)
         -> Array a
         -> Array b
         -> Array c
         -> Array d
zipWith3 f as bs = A.zipWith ($) (A.zipWith f as bs)

-- | `replicateA_ n m` repeats an Applicative action `m` `n` times and
-- | discards the result.
replicateA_ :: forall m a. Applicative m => Int -> m a -> m Unit
replicateA_ n m = traverse_ (\_ -> m) (1..n)
