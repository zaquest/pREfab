module Put
  ( Output
  , newOutput
  , PUT
  , PutM
  , runPut
  , Put
  , Putter
  , putWord8
  , putWord16le
  , putWord32le
  , putInt32le
  , putASCII
  , putASCIINul
  ) where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)

-- | Binary output stream (ArrayBuffer inside)
foreign import data Output :: Type

-- | Create new output stream.
foreign import newOutput :: forall e. Eff ( put :: PUT | e ) Output

-- | The `PUT` effect represents computations that write to `Output`.
foreign import data PUT :: Effect

-- | Put monad
newtype PutM a = Put (forall e. Output -> Eff ( put :: PUT | e ) a)

-- | `runPut putter out` runs `putter` put action on the `out` output
-- | stream.
runPut :: forall e a. PutM a -> Output -> Eff ( put :: PUT | e ) a
runPut (Put f) = f

-- | Conveniece type alias. It's not common to have a putter that
-- | returns something useful.
type Put = PutM Unit

-- | Type of a function that creates a put action from a value.
type Putter a = a -> Put

instance putFunctor :: Functor PutM where
  map f (Put p) = Put (\ab -> f <$> p ab)

instance putApply :: Apply PutM where
  apply (Put pf) (Put pa) = Put (\ab -> pf ab <*> pa ab)

instance putApplicative :: Applicative PutM where
  pure a = Put (\_ -> pure a)

instance putBind :: Bind PutM where
  bind (Put pa) f = Put (\ab ->  do a <- pa ab
                                    runPut (f a) ab)

instance putMonad :: Monad PutM

type JSMethodName = String

type Endianness = Boolean

littleEndian :: Endianness
littleEndian = true

bigEndian :: Endianness
bigEndian = false

type ByteLength = Int

foreign import putter :: forall a
                       . JSMethodName -- ^ DataView setter name
                      -> ByteLength -- ^ Length of a type in bytes
                      -> Endianness -- ^ Order of bytes to use
                      -> Putter a

-- | Put 1 byte unsiged integer to an output stream.
putWord8 :: Putter Int
putWord8 = putter "setUint8" 1 false

-- | Put 2 byte unsiged integer using little endian byte order to an
-- | output stream.
putWord16le :: Putter Int
putWord16le = putter "setUint16" 2 littleEndian

-- | Put a 4 byte unsiged integer using little endian byte order to an
-- | output stream.
putWord32le :: Putter Int
putWord32le = putter "setUint32" 4 littleEndian

-- | Put a 4 byte siged integer using little endian byte order to an
-- | output stream.
putInt32le :: Putter Int
putInt32le = putter "setInt32" 4 littleEndian

-- | Put ASCII string (one byte per character) *without* a terminating
-- | null-byte character to an output string.
foreign import putASCII :: Putter String

-- | Put ASCII string (one byte per character) *with* a terminating
-- | null-byte character to an output string.
putASCIINul :: Putter String
putASCIINul s = putASCII s *> putWord8 0
