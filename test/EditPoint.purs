module Test.EditPoint where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Foldable (all)
import Data.Array (zipWith)

import Test.Assert (ASSERT, assert)

import Data.Polygon (Poly, mkPoly)
import Linear.R2 (p2)
--import Drag.EditPoint (polyEditPoints)
import Editor.WorkArea (editPointEq)

-- testPolyEditPoints :: forall a. Eff (assert :: ASSERT | a) Unit
-- testPolyEditPoints = do
--   assert (all id (zipWith editPointEq epoints (polyEditPoints poly)))
--   where poly = Poly (fromFoldable [p2 0 0, p2 2 0, p2 1 1])
--         epoints = [{ before : p2 1 1
--                    , point  : p2 0 0
--                    , after  : p2 2 0 }
--                   ,{ before : p2 0 0
--                    , point  : p2 2 0
--                    , after  : p2 1 1 }
--                   ,{ before : p2 2 0
--                    , point  : p2 1 1
--                    , after  : p2 0 0 }]
