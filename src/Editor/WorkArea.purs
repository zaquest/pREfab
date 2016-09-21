module Editor.WorkArea where

import Data.Eq (class Eq, (==))
import Data.BooleanAlgebra ((&&))
import Data.Maybe (Maybe(..))
import Linear.R2 (P2)
import Data.Polygon (Poly2)
import Grid (CGrid)
import RedEclipse.Face (Validity, validatePoly)

type EditPoint a = { before :: P2 a
                   , point  :: P2 a
                   , after  :: P2 a }

editPointEq :: forall a. Eq a => EditPoint a -> EditPoint a -> Boolean
editPointEq p1 p2 = p1.before == p2.before && p1.point == p2.point && p1.after == p2.after

--instance editPointEq :: Eq a => Eq (EditPoint a) where
--  eq p1 p2 = p1.before == p2.before && p1.point == p2.point && p1.after == p2.after

type WorkArea = { poly :: Poly2 CGrid
                , point :: Maybe (EditPoint CGrid)
                , validity :: Maybe Validity }

mkWorkArea :: Poly2 CGrid -> WorkArea
mkWorkArea poly = { poly: poly
                  , point: Nothing
                  , validity: validatePoly poly }
