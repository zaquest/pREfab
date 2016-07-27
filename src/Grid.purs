module Grid where

import Prelude

-- | Grid coordinate type.
type CGrid = Int

gridSize :: CGrid
gridSize = 8

-- | Find previous/next point on the grid
gridPrev :: CGrid -> CGrid
gridPrev x = (x `div` gridSize) * gridSize - (if x < 0 then gridSize else 0)

gridNext :: CGrid -> CGrid
gridNext x = gridPrev (x + (gridSize - 1))
