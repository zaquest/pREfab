module Test.Data.Corner where

import Prelude
import Data.Corner (Corner(..), isLine, clockWise, counterClockWise)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert

import Linear.R2 (p2)

cornerSuite :: forall a. TestSuite a
cornerSuite = suite "Corner" do

  -- same as Test.Data.Segment.onLine
  test "isLine" do
    Assert.assert "a line" (isLine (Corner (p2 0.0 0.0)
                                           (p2 8.0 8.0)
                                           (p2 4.0 4.0)))
    Assert.assertFalse "not a line"
                       (isLine (Corner (p2 0.0 8.0)
                                       (p2 0.0 0.0)
                                       (p2 8.0 8.0)))

  -- same as Test.Data.Segment.inside
  test "clockWise" do
    Assert.assert "forward" (clockWise (Corner (p2 8 8)
                                               (p2 0 0)
                                               (p2 0 8)))
    Assert.assert "backward" (clockWise (Corner (p2 0 0)
                                                       (p2 8 8)
                                                       (p2 8 0)))

  -- same as Test.Data.Segment.outside
  test "counterClockWise" do
    Assert.assert "forward" (counterClockWise (Corner (p2 8 8)
                                                      (p2 0 0)
                                                      (p2 8 0)))
    Assert.assert "backward" (counterClockWise (Corner (p2 0 0)
                                                       (p2 8 8)
                                                       (p2 0 8)))
