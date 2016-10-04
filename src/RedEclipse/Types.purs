module RedEclipse.Types where

import Data.String (length)
import Control.Apply ((*>))
import Put (Putter, putWord16le, putInt32le, putASCII, putASCIINul)

type MapzInt = Int -- ^ Int32

putMapzInt :: Putter MapzInt
putMapzInt = putInt32le

type MapzUShort = Int -- ^ UInt16

putMapzUShort :: Putter MapzUShort
putMapzUShort = putWord16le

type Texture = MapzUShort

putTexture :: Putter Texture
putTexture = putMapzUShort

type MapzString = String

putMapzString :: Putter MapzString
putMapzString = putASCII

putMapzStringNul :: Putter MapzString
putMapzStringNul = putASCIINul

putMapzStringLen :: Putter MapzString
putMapzStringLen s = putMapzInt (length s) *> putMapzStringNul s
