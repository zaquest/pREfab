module Put where

import Prelude
import Control.Monad.Eff (Eff)

foreign import data PUT :: !

foreign import data Output :: *

newtype PutM a = Put (forall e. Output -> Eff ( put :: PUT | e ) a)

runPut :: forall e a. PutM a -> Output -> Eff ( put :: PUT | e ) a
runPut (Put f) = f

type Put = PutM Unit

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

foreign import newOutput :: forall e. Eff ( put :: PUT | e ) Output

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

putWord8 :: Putter Int
putWord8 = putter "setUint8" 1 false

putWord16le :: Putter Int
putWord16le = putter "setUint16" 2 littleEndian

putWord32le :: Putter Int
putWord32le = putter "setUint32" 4 littleEndian

putInt32le :: Putter Int
putInt32le = putter "setInt32" 4 littleEndian

foreign import putASCII :: Putter String

putASCIINul :: Putter String
putASCIINul s = putASCII s *> putWord8 0
