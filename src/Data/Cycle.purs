module Cycle where

import Data.Array (Array(..))

data Cycle a = Cycle Int (Array a)
