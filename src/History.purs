module History
  ( History
  , start
  , undo
  , redo
  , push
  , present
  , undoSize
  , redoSize
  ) where

import Data.Ring ((+), (-))
import Data.Ord ((>))
import Data.Maybe (Maybe(..))
import Data.Generic (class Generic)
import Data.List (List(..), take)

-- | History data type of a limited size with automatic recycling when
-- | maximum history length is exceeded.
newtype History a = History
  -- | Undo queue
  { prev :: List a
  -- | Current item
  , cur :: a
  -- | Redo queue
  , next :: List a
  -- | Redo (next) list length
  , nextSize :: Int
  -- | Maximal length at which recycling occurs
  , maxSize :: Int
  -- | Length immediately after recycling
  , recycleSize :: Int
  -- | Total history length
  , size :: Int }

-- | `start first recycle max` creates a new history starting with
-- | `first`. `max` is the history length at which recycling happens.
-- | `recycle` is the history length immediately after recycling.
start :: forall a. a -> Int -> Int -> History a
start first recycle max = History { prev: Nil
                                  , cur: first
                                  , next: Nil
                                  , nextSize: 0
                                  , maxSize: max
                                  , recycleSize: recycle
                                  , size: 1 }

derive instance genericHistory :: Generic a => Generic (History a)

-- | `undo hist` moves current state to redo queue and sets previous
-- | element to be a present element. Returns updated history `state`
-- | and a previous value `value`, which is `Nothing` if undo queue
-- | was empty.
undo :: forall a. History a -> { state :: History a, value :: Maybe a }
undo h@(History {prev: Nil}) = { state: h, value: Nothing }
undo (History h@{prev: Cons p ps, cur, next, nextSize}) =
  { state: History h { prev = ps
                     , cur = p
                     , next = Cons cur next
                     , nextSize = nextSize + 1 }
  , value: Just p }

-- | `redo hist` moves current state to undo queue and sets next
-- | element to be a present element. Returns updated history `state`
-- | and a next value `value`, which is `Nothing` if redo queue was
-- | empty.
redo :: forall a. History a -> { state :: History a, value :: Maybe a }
redo h@(History {next: Nil}) = { state: h, value: Nothing }
redo (History h@{prev, cur, next: Cons p ps, nextSize}) =
  { state: History h { prev = Cons cur prev
                     , cur = p
                     , next = ps
                     , nextSize = nextSize - 1 }
  , value: Just p }

-- | `push hist val` pushes current state to undo queue, empties redo
-- | queue and sets present element to be `val`. Returns updated
-- | queue. If new history size exceeds maximum history size recyling
-- | is done.
push :: forall a. History a -> a -> History a
push (History history) p =
  let size = history.size - history.nextSize + 1
      ps = if size > history.maxSize
              then { prev: take history.recycleSize history.prev
                   , size: history.recycleSize }
              else { prev: history.prev
                   , size: size }
   in History history { prev = Cons history.cur ps.prev
                      , cur = p
                      , next = Nil
                      , nextSize = 0
                      , size = ps.size }

-- | `present hist` returns a present moment element.
present :: forall a. History a -> a
present (History {cur}) = cur

-- | `undoSize hist` returns a number of elements in undo queue.
undoSize :: forall a. History a -> Int
undoSize (History h) = h.size - h.nextSize - 1

-- | `redoSize hist` returns a number of elements in redo queue.
redoSize :: forall a. History a -> Int
redoSize (History h) = h.nextSize
