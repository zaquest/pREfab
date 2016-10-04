module Serialize where

import Put (Putter)

class Serialize a where
  put :: Putter a
