module Storage
  ( loadHistory
  , saveHistory
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Monad.Eff (Eff)
import Data.Generic (class Generic)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)

import DOM (DOM)
import DOM.WebStorage.Storage (getItem, setItem, Storage)

import History (History)
import Data.Polygon (Poly2)
import Grid (CGrid)

eitherToMaybe :: forall a b. Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v

-- | Storage keys phantom type.
data Key a = HistoryKey

derive instance genericKey :: Generic (Key a)

-- | Concrete history key.
historyKey :: Key (History (Poly2 CGrid))
historyKey = HistoryKey

toJSONGeneric :: forall a. Generic a => a -> String
toJSONGeneric = stringify <<< gEncodeJson

fromJSONGeneric :: forall a. Generic a => String -> Maybe a
fromJSONGeneric s = do
  json <- eitherToMaybe (jsonParser s)
  eitherToMaybe (gDecodeJson json)

getItem' :: forall key a e
          . Generic (key a)
         => Generic a
         => Storage
         -> key a
         -> Eff (dom :: DOM | e) (Maybe a)
getItem' storage key = do
  mstr <- getItem (toJSONGeneric key) storage
  case mstr of
    Nothing -> pure Nothing
    Just s -> pure (fromJSONGeneric s)

setItem' :: forall key a e
          . Generic (key a)
         => Generic a
         => Storage
         -> key a
         -> a
         -> Eff (dom :: DOM | e) Unit
setItem' storage key val = setItem (toJSONGeneric key) (toJSONGeneric val) storage

-- | `loadHisotry storage` loads polygon editing history from local
-- | storage `storage`. Returns `Nothing` if history is not present in
-- | the `storage`.
loadHistory :: forall e
             . Storage
            -> Eff ( dom :: DOM | e ) (Maybe (History (Poly2 CGrid)))
loadHistory storage = getItem' storage historyKey

-- | `saveHistory storage hist` saves polygon editing history `hist`
-- | to local storage `storage`.
saveHistory :: forall e
             . Storage
            -> History (Poly2 CGrid)
            -> Eff ( dom :: DOM | e ) Unit
saveHistory storage = setItem' storage historyKey
