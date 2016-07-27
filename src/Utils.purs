module Utils
     ( enumerate
     , foldlA2
     , whileM_
     ) where

import Prelude
import Data.Traversable (class Traversable, for)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Control.Monad.Transformerless.State (evalState, put, get)

foldlA2 :: forall t f a. (Foldable t, Applicative f) => (a -> a -> a) -> t (f a) -> Maybe (f a)
foldlA2 f = foldl af Nothing
  where af Nothing a = Just a
        af (Just b) a = Just (f <$> b <*> a)

whileM_ :: forall a m. Monad m => a -> (a -> Boolean) -> (a -> m a) -> m Unit
whileM_ start test body | test start = do next <- body start
                                          whileM_ next test body
                        | otherwise = pure unit

enumerate :: forall a t. Traversable t => t a -> t { idx :: Int, elem :: a}
enumerate xs = flip evalState 0 <<< for xs $ \x -> do
                 idx <- get
                 put (idx + 1)
                 pure { idx: idx, elem: x }
