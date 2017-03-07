module Serialize
  ( class Serialize
  , put
  ) where

import Put (Putter)

-- | Type class for serializable data types.
-- |
-- | * `put` puts a value of type `a` to an output stream.
class Serialize a where
  put :: Putter a
