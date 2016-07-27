module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)

import Test.Assert (ASSERT, assert)

main :: forall a. Eff (assert :: ASSERT | a) Unit
main = do
  assert true
