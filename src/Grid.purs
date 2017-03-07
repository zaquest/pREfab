-- | There're two grids: subgrid and main grid. Each main grid cell
-- | is a square covered with `gridSize * gridSize` subgrid cells. All
-- | operations are done on subgrid coordiantes unless specified
-- | otherwise.
module Grid
  ( CGrid
  , gridSize
  , gridPrev
  , gridNext
  , fromGrid
  , toGrid
  ) where

import Prelude
import Linear.R2 (P2, invY)
import Linear.Vector ((^*), (^/))
import Data.Int (toNumber, round)

-- | Grid coordinate type.
type CGrid = Int

-- | Number of subgrid cells along each dimension a main grid cell.
-- | `gridSize = 8` since we're dealing with the Cube engine and an
-- | octree.
gridSize :: Int
gridSize = 8

-- | `gridPrev x` finds a coordiante of a previous closest main grid
-- | cell relative to subgrid cell with coordinate `x`.
gridPrev :: CGrid -> CGrid
gridPrev x = (x `div` gridSize) * gridSize - (if x `mod` gridSize < 0 then gridSize else 0)

-- | `gridNext x` finds a coordiante of a next closest main grid cell
-- | relative to subgrid cell with coordinate `x`.
gridNext :: CGrid -> CGrid
gridNext x = gridPrev (x + (gridSize - 1))

-- | `fromGrid o gridSz p` converts a point `p` from grid coordinates
-- | frame with origin `o` and a cell size `gridSz`.
fromGrid :: P2 Number -> Number -> P2 CGrid -> P2 Number
fromGrid o gridSz p = o + (toNumber <$> invY p) ^* gridSz

-- | `toGrid o gridSz p` converts a point `p` to grid coordinates
-- | frame with origin `o` and a cell size `gridSz`.
toGrid :: P2 Number -> Number -> P2 Number -> P2 CGrid
toGrid o gridSz p = invY (round <$> ((p - o) ^/ gridSz))
