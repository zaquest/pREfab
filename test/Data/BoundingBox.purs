module Test.Data.BoundingBox where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Polygon (mkPoly, points)
import Data.BoundingBox (boundingBox, boundingRect)
import Utils (fromJust)

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert

import Linear.R2 (p2)

boundingBoxSuite :: forall a. TestSuite a
boundingBoxSuite = suite "BoundingBox" do

  let poly1 = fromJust $ mkPoly [p2 0 0, p2 0 8, p2 8 8, p2 8 0]
  let poly2 = fromJust $ mkPoly [p2 4 0, p2 0 4, p2 4 8, p2 8 4]

  test "box" do
    let bb1 = boundingBox poly1
    let bb2 = boundingBox poly2
    Assert.equal bb1.lo bb2.lo
    Assert.equal bb1.hi bb2.hi

  test "rect" do
    let br1 = boundingRect poly1
    let br2 = boundingRect poly2
    Assert.equal (points br1) (points br2)
