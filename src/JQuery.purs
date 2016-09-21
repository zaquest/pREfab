module JQuery where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (JQueryEvent)
import DOM (DOM)

foreign import clientX
  :: forall e
   . JQueryEvent
  -> Eff (dom :: DOM | e) Number

foreign import clientY
  :: forall e
   . JQueryEvent
  -> Eff (dom :: DOM | e) Number
