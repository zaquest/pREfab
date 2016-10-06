module History where

import Data.Maybe (Maybe(..))
import Data.Generic (class Generic)
import Data.List (List(..))

newtype History a = History { prev :: List a
                            , cur :: a
                            , next :: List a }

start :: forall a. a -> History a
start first = History { prev: Nil, cur: first, next: Nil }

derive instance genericHistory :: Generic a => Generic (History a)

undo :: forall a. History a -> { state :: History a, value :: Maybe a }
undo h@(History {prev: Nil}) = { state: h, value: Nothing }
undo (History h@{prev: Cons p ps, cur, next}) =
      { state: History h {prev = ps, cur = p, next = Cons cur next}
      , value: Just p }

redo :: forall a. History a -> { state :: History a, value :: Maybe a }
redo h@(History {next: Nil}) = { state: h, value: Nothing }
redo (History h@{prev, cur, next: Cons p ps}) =
  { state: History h {prev = Cons cur prev, cur = p, next = ps}
  , value: Just p }

-- TODO: limit history length
push :: forall a. History a -> a -> History a
push (History h@{prev, cur}) p = History h { prev = Cons cur prev
                                           , cur = p
                                           , next = Nil }

present :: forall a. History a -> a
present (History {cur}) = cur
