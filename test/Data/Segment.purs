module Test.Data.Segment where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Segment ( Seg(..), inside, outside, intersection, onLine
                    , distance, nearestPoint )
import Math (sqrt)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert

import Linear.R2 (p2)

segmentSuite :: forall a. TestSuite a
segmentSuite = suite "Segment" do

  -- same as Test.Data.Corner.isLine
  test "onLine" do
    Assert.assert "on line" (onLine (p2 4.0 4.0)
                                    (Seg (p2 0.0 0.0) (p2 8.0 8.0)))
    Assert.assertFalse "not on line"
                       (onLine (p2 0.0 8.0)
                               (Seg (p2 0.0 0.0) (p2 8.0 8.0)))

  -- same as Test.Data.Corner.clockWise
  test "inside" do
    Assert.assert "forward" (inside (p2 0 8) (Seg (p2 8 8) (p2 0 0)))
    Assert.assert "backward" (inside (p2 8 0) (Seg (p2 0 0) (p2 8 8)))

  -- same as Test.Data.Corner.counterClockWise
  test "outside" do
    Assert.assert "forward" (outside (p2 8 0) (Seg (p2 8 8) (p2 0 0)))
    Assert.assert "backward" (outside (p2 0 8) (Seg (p2 0 0) (p2 8 8)))

  test "intersection" do
    -- segments intersect
    Assert.equal (Just (p2 4.0 4.0))
                 (intersection (Seg (p2 0.0 0.0) (p2 8.0 8.0))
                               (Seg (p2 0.0 8.0) (p2 8.0 0.0)))
    -- lines intersect
    Assert.equal (Just (p2 8.0 8.0))
                 (intersection (Seg (p2 0.0 0.0) (p2 4.0 4.0))
                               (Seg (p2 0.0 16.0) (p2 4.0 12.0)))
    -- parallel lines
    Assert.equal Nothing
                 (intersection (Seg (p2 0.0 0.0) (p2 8.0 8.0))
                               (Seg (p2 0.0 8.0) (p2 8.0 16.0)))

  suite "distance" do

    test "segment end is closer" do
      Assert.equal 1.0 (distance (p2 0.0 0.0)
                                 (Seg (p2 1.0 0.0) (p2 8.0 8.0)))

    test "point projection is closer" do
      Assert.equal (sqrt 128.0 / 2.0)
                   (distance (p2 0.0 8.0)
                             (Seg (p2 0.0 0.0) (p2 8.0 8.0)))

  suite "nearestPoint" do

    test "segment end is closer" do
      Assert.equal (p2 1.0 0.0)
                   (nearestPoint (p2 0.0 0.0)
                                 (Seg (p2 1.0 0.0) (p2 8.0 8.0)))

    test "point projection is closer" do
      Assert.equal (p2 4.0 4.0)
                   (nearestPoint (p2 0.0 8.0)
                                 (Seg (p2 0.0 0.0) (p2 8.0 8.0)))
