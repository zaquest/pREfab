module Window where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Int (toNumber)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Window (innerWidth, innerHeight) as DOM
import DOM.Event.Types (Event)

getSize :: forall e. Eff ( dom :: DOM | e ) { width :: Number, height :: Number }
getSize = do
  window <- DOM.window
  width <- toNumber <$> DOM.innerWidth window
  height <- toNumber <$> DOM.innerHeight window
  pure { width, height }

foreign import onResize :: forall e. (Event -> Eff e Unit) -> Eff ( dom :: DOM | e ) Unit
