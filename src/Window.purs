module Window
  ( getSize
  , onResize
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Int (toNumber)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Window (innerWidth, innerHeight) as DOM
import DOM.Event.Types (Event)

-- | Get browser's window inner width and height.
getSize :: forall e
         . Eff ( dom :: DOM | e ) { width :: Number
                                  , height :: Number }
getSize = do
  window <- DOM.window
  width <- toNumber <$> DOM.innerWidth window
  height <- toNumber <$> DOM.innerHeight window
  pure { width, height }

-- Had to add DOM to the passed function. Why?
-- | `onResize fn` sets a handler `fn` to be executed on browser's
-- | window resize.
foreign import onResize :: forall e
                         . (Event -> Eff ( dom :: DOM | e ) Unit)
                        -> Eff ( dom :: DOM | e ) Unit
