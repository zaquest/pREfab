module Storage
  ( loadHistory
  , saveHistory
  ) where

import Prelude

import Data.Maybe (Maybe)
import Control.Monad.Eff (Eff)
import Data.Generic (class Generic)

import DOM.WebStorage (STORAGE, getItem, setItem, ForeignStorage)

import History (History)
import Data.Polygon (Poly2)
import Grid (CGrid)

-- | Storage keys phantom type.
data Key a = HistoryKey

derive instance genericKey :: Generic (Key a)

-- | Concrete history key.
historyKey :: Key (History (Poly2 CGrid))
historyKey = HistoryKey

-- | `loadHisotry storage` loads polygon editing history from local
-- | storage `storage`. Returns `Nothing` if history is not present in
-- | the `storage`.
loadHistory :: forall e
             . ForeignStorage
             -> Eff ( storage :: STORAGE | e ) (Maybe (History (Poly2 CGrid)))
loadHistory storage = getItem storage historyKey

-- | `saveHistory storage hist` saves polygon editing history `hist`
-- | to local storage `storage`.
saveHistory :: forall e
             . ForeignStorage
            -> History (Poly2 CGrid)
            -> Eff ( storage :: STORAGE | e ) Unit
saveHistory storage = setItem storage historyKey
