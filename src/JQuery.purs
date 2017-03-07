module JQuery
  ( clientX
  , clientY
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (JQueryEvent)
import DOM (DOM)

-- | `clientX jqEvent`, where `jqEvent` is a mouse event, returns
-- | mouse `x` coordinate at the moment when event took place.
foreign import clientX
  :: forall e
   . JQueryEvent
  -> Eff (dom :: DOM | e) Number

-- | `clientX jqEvent`, where `jqEvent` is a mouse event, returns
-- | mouse `y` coordinate at the moment when event took place.
foreign import clientY
  :: forall e
   . JQueryEvent
  -> Eff (dom :: DOM | e) Number
