module Grid where

import Prelude
import Linear.R2 (P2, invY, (^*), (^/))
import Data.Int (toNumber, round)

-- | Grid coordinate type.
type CGrid = Int

gridSize :: CGrid
gridSize = 8

-- | Find previous/next point on the grid
gridPrev :: CGrid -> CGrid
gridPrev x = (x `div` gridSize) * gridSize - (if x < 0 then gridSize else 0)

gridNext :: CGrid -> CGrid
gridNext x = gridPrev (x + (gridSize - 1))

fromGrid :: P2 Number -> Number -> P2 CGrid -> P2 Number
fromGrid o gridSz p = o + (toNumber <$> invY p) ^* gridSz

toGrid :: P2 Number -> Number -> P2 Number -> P2 CGrid
toGrid o gridSz p = invY (round <$> ((p - o) ^/ gridSz))
