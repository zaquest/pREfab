module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)

import Test.Assert (ASSERT, assert)
import Test.EditPoint (testPolyEditPoints)
import Test.Polygon (testOnLine)

main :: forall a. Eff (assert :: ASSERT | a) Unit
main = do
  testPolyEditPoints
  testOnLine
