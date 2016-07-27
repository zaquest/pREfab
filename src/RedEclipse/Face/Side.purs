module RedEclipse.Face.Side
     ( Side(..)
     , allSides
     ) where

import Prelude
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..))
import Data.Maybe (Maybe(..))
import Data.Function (on)

data Side = SUp | SRight | SDown | SLeft

sideToInt :: Side -> Int
sideToInt SUp = 1
sideToInt SRight = 2
sideToInt SDown = 3
sideToInt SLeft = 4

intToSide :: Int -> Maybe Side
intToSide 1 = Just SUp
intToSide 2 = Just SRight
intToSide 3 = Just SDown
intToSide 4 = Just SLeft
intToSide _ = Nothing

instance sideEq :: Eq Side where
  eq SUp SUp = true
  eq SRight SRight = true
  eq SDown SDown = true
  eq SLeft SLeft = true
  eq _ _ = false

instance sideOrd :: Ord Side where
  compare = compare `on` sideToInt

instance sideEnum :: Enum Side where
  succ SUp    = Just SRight
  succ SRight = Just SDown
  succ SDown  = Just SLeft
  succ SLeft  = Nothing

  pred SUp    = Nothing
  pred SRight = Just SUp
  pred SDown  = Just SRight
  pred SLeft  = Just SDown

instance sideBounded :: Bounded Side where
  bottom = SUp
  top = SLeft

instance sideBoundedEnum :: BoundedEnum Side where
  fromEnum = sideToInt
  toEnum = intToSide
  cardinality = Cardinality 4

instance sideShow :: Show Side where
  show SUp = "SUp"
  show SRight = "SRight"
  show SDown = "SDown"
  show SLeft = "SLeft"

allSides :: Array Side
allSides = [SUp, SRight, SDown, SLeft]
