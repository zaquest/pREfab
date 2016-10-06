module Storage where

import Prelude

import Data.Maybe (Maybe)
import Control.Monad.Eff (Eff)
import Data.Generic (class Generic)

import DOM.WebStorage (STORAGE, getItem, setItem, ForeignStorage)

import History (History)
import Data.Polygon (Poly2)
import Grid (CGrid)

data Key a = HistoryKey

derive instance genericKey :: Generic (Key a)

historyKey :: Key (History (Poly2 CGrid))
historyKey = HistoryKey

loadHistory :: forall e
             . ForeignStorage
             -> Eff ( storage :: STORAGE | e ) (Maybe (History (Poly2 CGrid)))
loadHistory storage = getItem storage historyKey

saveHistory :: forall e
             . ForeignStorage
            -> History (Poly2 CGrid)
            -> Eff ( storage :: STORAGE | e ) Unit
saveHistory storage = setItem storage historyKey
