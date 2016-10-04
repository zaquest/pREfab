module RedEclipse.Cube where

import Prelude
import Data.Semigroup ((<>))
import Data.Int.Bits ((.|.), (.&.), shl)
import Put (putWord8)
import Serialize (class Serialize)

newtype Edge = Edge { offset :: Int, limit :: Int }

derive instance edgeEq :: Eq Edge

packEdge :: Edge -> Int
packEdge (Edge e) = ((e.limit .&. 0xf) `shl` 4) .|. (e.offset .&. 0xf)

instance serializeEdge :: Serialize Edge where
  put = putWord8 <<< packEdge

instance showEdge :: Show Edge where
  show (Edge e) = "(Edge " <> show e.offset <> " " <> show e.limit <> ")"

emptyEdge :: Edge
emptyEdge = Edge { offset: 0, limit: 0 }

solidEdge :: Edge
solidEdge = Edge { offset: 0, limit: 8 }

{-
     7      6
     +------+
    /|     /|
  5/ |   4/ |
  +------+  |
  |  +---|--+
  | /3   | /2
  |/     |/
  +------+
  1      0
-}
newtype Cube = Cube { edge01 :: Edge, edge23 :: Edge, edge45 :: Edge, edge67 :: Edge
                    , edge02 :: Edge, edge46 :: Edge, edge13 :: Edge, edge57 :: Edge
                    , edge04 :: Edge, edge15 :: Edge, edge26 :: Edge, edge37 :: Edge }

edges :: Cube -> Array Edge
edges (Cube c) = [ c.edge01, c.edge23, c.edge45, c.edge67
                 , c.edge02, c.edge46, c.edge13, c.edge57
                 , c.edge04, c.edge15, c.edge26, c.edge37 ]

uniCube :: Edge -> Cube
uniCube e = Cube { edge01: e, edge23: e, edge45: e, edge67: e
                 , edge02: e, edge46: e, edge13: e, edge57: e
                 , edge04: e, edge15: e, edge26: e, edge37: e }

emptyCube :: Cube
emptyCube = uniCube emptyEdge

solidCube :: Cube
solidCube = uniCube solidEdge
