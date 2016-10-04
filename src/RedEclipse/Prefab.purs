module RedEclipse.Prefab where

import Control.Apply ((*>))
import Control.Bind (bind)
import Data.Traversable (traverse_)
import Serialize (class Serialize, put)
import Put (putWord8)
import RedEclipse.Types ( MapzString, MapzInt, putMapzInt
                        , putMapzUShort, putMapzString )
import RedEclipse.Cube (Cube, edges)
import RedEclipse.Octree (Octree(..), children)
import Utils (replicateA_)

newtype MapzIVec3 = MapzIVec3 { x :: MapzInt
                              , y :: MapzInt
                              , z :: MapzInt }

mapzIVec3 :: MapzInt -> MapzInt -> MapzInt -> MapzIVec3
mapzIVec3 x y z = MapzIVec3 { x, y, z }

instance serializeMpaIVec3 :: Serialize MapzIVec3 where
  put (MapzIVec3 v) = traverse_ putMapzInt [v.x, v.y, v.z]

data PrefabFileHeader = PrefabFileHeader { magic   :: MapzString
                                         , version :: MapzInt }

defaultPrefabFileHeader :: PrefabFileHeader
defaultPrefabFileHeader = PrefabFileHeader { magic: "OEBR"
                                           , version: 0 }

instance serializePrefabFileHeader :: Serialize PrefabFileHeader where
  put (PrefabFileHeader hdr) = do
    putMapzString hdr.magic
    putMapzInt hdr.version

newtype PrefabHeader = PrefabHeader { orients :: MapzIVec3
                                    , sizes   :: MapzIVec3
                                    , grid    :: MapzInt
                                    , orient  :: MapzInt }

instance serializePrefabHeader :: Serialize PrefabHeader where
  put (PrefabHeader phdr) = do
    put phdr.orients
    put phdr.sizes
    putMapzInt phdr.grid
    putMapzInt phdr.orient

-- | Array of cubes of size `PrefabHeader.sizes.x * PrefabHeader.sizes.y * PrefabHeader.sizes.z`
newtype PrefabGeom = PrefabGeom (Array (Octree Cube))

instance serializePrefabGeom :: Serialize PrefabGeom where
  put (PrefabGeom cubes) = traverse_ putOctree cubes
    where
      putOctree (Leaf cube) = do
        putMapzUShort 0
        traverse_ put (edges cube)
        replicateA_ 6 (putMapzUShort 1)
      putOctree node = putWord8 0xff *> traverse_ putOctree (children node)

newtype Prefab = Prefab { header :: PrefabHeader
                        , geom   :: PrefabGeom }

instance serializePrefab :: Serialize Prefab where
  put (Prefab prefab) = do
    put prefab.header
    put prefab.geom

newtype PrefabFile = PrefabFile { header :: PrefabFileHeader
                                , prefab :: Prefab }

instance serializePrefabFile :: Serialize PrefabFile where
  put (PrefabFile pfile) = do
    put pfile.header
    put pfile.prefab
