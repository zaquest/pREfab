module History where

import Data.Ring ((+), (-))
import Data.Ord ((>))
import Data.Maybe (Maybe(..))
import Data.Generic (class Generic)
import Data.List (List(..), take)

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

start :: forall a. a -> Int -> Int -> History a
start first recycle max = History { prev: Nil
                                  , cur: first
                                  , next: Nil
                                  , nextSize: 0
                                  , maxSize: max
                                  , recycleSize: recycle
                                  , size: 1 }

derive instance genericHistory :: Generic a => Generic (History a)

undo :: forall a. History a -> { state :: History a, value :: Maybe a }
undo h@(History {prev: Nil}) = { state: h, value: Nothing }
undo (History h@{prev: Cons p ps, cur, next, nextSize}) =
  { state: History h { prev = ps
                     , cur = p
                     , next = Cons cur next
                     , nextSize = nextSize + 1 }
  , value: Just p }

redo :: forall a. History a -> { state :: History a, value :: Maybe a }
redo h@(History {next: Nil}) = { state: h, value: Nothing }
redo (History h@{prev, cur, next: Cons p ps, nextSize}) =
  { state: History h { prev = Cons cur prev
                     , cur = p
                     , next = ps
                     , nextSize = nextSize - 1 }
  , value: Just p }

push :: forall a. History a -> a -> History a
push (History history) p =
  let size = history.size - history.nextSize + 1
      prev = if size > history.maxSize
                then take history.recycleSize history.prev
                else history.prev
   in History history { prev = Cons history.cur prev
                      , cur = p
                      , next = Nil
                      , nextSize = 0
                      , size = size }

present :: forall a. History a -> a
present (History {cur}) = cur

undoSize :: forall a. History a -> Int
undoSize (History h) = h.size - h.nextSize - 1

redoSize :: forall a. History a -> Int
redoSize (History h) = h.nextSize
