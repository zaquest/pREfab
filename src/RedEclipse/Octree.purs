module RedEclipse.Octree where

import Control.Apply ((*>))
import Data.Traversable (traverse_)
import Serialize (class Serialize, put)
import Put (Putter, putWord8)
import RedEclipse.Cube (Cube, solidCube, emptyCube)

data Octree a = Leaf a
              | Node { sed :: Octree a
                     , swd :: Octree a
                     , ned :: Octree a
                     , nwd :: Octree a
                     , seu :: Octree a
                     , swu :: Octree a
                     , neu :: Octree a
                     , nwu :: Octree a }

children :: forall a. Octree a -> Array (Octree a)
children (Leaf _) = []
children (Node n) = [n.sed, n.swd, n.ned, n.nwd, n.seu, n.swu, n.neu, n.nwu]

putRoot :: forall a. Serialize a => Putter (Octree a)
putRoot (Node n) = traverse_ put [n.sed, n.swd, n.ned, n.nwd, n.seu, n.swu, n.neu, n.nwu]
putRoot (Leaf n) = put n

instance serializeOctree :: Serialize a => Serialize (Octree a) where
  put n@(Node _) = putWord8 0 *> putRoot n
  put (Leaf cube) = put cube

defaultOctree :: Octree Cube
defaultOctree = Node { sed: lsc, swd: lsc, ned: lsc, nwd: lsc
                     , seu: lec, swu: lec, neu: lec, nwu: lec }
  where lsc = Leaf solidCube
        lec = Leaf emptyCube
