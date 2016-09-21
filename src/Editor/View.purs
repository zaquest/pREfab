module Editor.View
     ( View
     , mkView
     , toGrid
     , fromGrid
     , subGridSize
     , mainGridSize
     ) where

import Data.Semiring ((*))
import Data.Int (toNumber)

import Grid (CGrid)
import Grid as G
import Linear.R2 (P2, p2, (^/))

type View = { width :: Number
            , height :: Number
            , origin :: P2 Number
            , zoom :: Number
            , gridSize :: Number }

mkView :: Number -> Number -> View
mkView w h = { width: w
             , height: h
             , origin: p2 w h ^/ 2.0
             , zoom: 1.0
             , gridSize: 8.0 }

toGrid :: View -> P2 Number -> P2 CGrid
toGrid view = G.toGrid view.origin (subGridSize view)

fromGrid :: View -> P2 CGrid -> P2 Number
fromGrid view = G.fromGrid view.origin (subGridSize view)

subGridSize :: View -> Number
subGridSize view = view.zoom * view.gridSize

mainGridSize :: View -> Number
mainGridSize view = view.zoom * view.gridSize * (toNumber G.gridSize)
