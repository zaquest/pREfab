module Ref
     ( REF
     , Ref
     , newRef
     , writeRef
     , readRef
     , modifyRef
     ) where

import Control.Monad.Eff (Eff)
--import Data.Unit (Unit)

-- | The `REF` effect represents those computations which access Ref.
foreign import data REF :: !

foreign import data Ref :: * -> *

foreign import newRef
  :: forall e a
   . a
  -> Eff (ref :: REF | e) (Ref a)

foreign import writeRef
  :: forall e a
   . Ref a
  -> a
  -> Eff (ref :: REF | e) a

foreign import readRef
  :: forall e a
   . Ref a
  -> Eff (ref :: REF | e) a

foreign import modifyRef
  :: forall e a
   . Ref a
  -> (a -> a)
  -> Eff (ref :: REF | e) a
