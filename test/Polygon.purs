module Test.Polygon where

import Prelude
import Control.Monad.Eff (Eff)

import Test.Assert (ASSERT, assert)

--import Data.Polygon (onLine)
--import Linear.R2 (p2)
--
--testOnLine :: forall a. Eff (assert :: ASSERT | a) Unit
--testOnLine = do
--  assert (onLine (p2 0 0) (p2 0 0) (p2 1 1))
