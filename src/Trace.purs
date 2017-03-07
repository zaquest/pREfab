module Trace
  ( trace
  , traceMsg
  ) where

-- | Debugging purposes function that doesn't require `Show` instance.
-- | Uses `console.log` internally.
foreign import trace :: forall a. a -> a

foreign import traceMsg :: forall a. String -> a -> a
