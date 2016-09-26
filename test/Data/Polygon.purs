module Test.Data.Polygon where

import Prelude
import Test.Unit (suite, test, failure, TestSuite)
import Test.Unit.Assert as Assert

import Data.Monoid ((<>))
import Data.Maybe (Maybe(..), isNothing)
import Data.Array (reverse, (\\))

import Linear.R2 (p2)
import Data.Polygon ( mkPoly, points, clockWise, square
                    , sutherlandHodgman, prunePoly, isConvex )
import Data.Segment (inside)
import Utils (fromJust)

polygonSuite :: forall a. TestSuite a
polygonSuite = suite "Polygon" do

  test "clockWise" do
    let fPoly = fromJust $ mkPoly [p2 0 0, p2 0 8, p2 8 8, p2 8 0]
    let rPoly = fromJust <<< mkPoly <<< reverse $ points fPoly
    Assert.assert "clock-wise" (clockWise fPoly)
    Assert.assertFalse "counter clock-wise" (clockWise rPoly)

  suite "sutherlandHodgman" do

    test "triangle intersection, touching additional point"
      let subjPoly = fromJust $ mkPoly [ p2 0.0 0.0
                                       , p2 0.0 16.0
                                       , p2 3.0 8.0
                                       , p2 8.0 8.0
                                       , p2 8.0 0.0 ]
          clipPoly = square 8.0 (p2 0.0 8.0)
          intersection = [p2 0.0 8.0, p2 0.0 16.0, p2 3.0 8.0]
       in case sutherlandHodgman inside clipPoly subjPoly of
            Nothing -> failure "failed to find an intersection"
            Just resultPoly -> do
              let result = points resultPoly
              Assert.equal [] (result \\ intersection)
              Assert.equal [] (intersection \\ result)

    -- the intersection should be the same as in sutherlandHodgman1
    -- but there were some issues with unrelated points interferring
    -- with intersection calculation hence the test
    test ( "triangle intersection, touching additional point, plus"
         <> " unrelated point" )
      let subjPoly = fromJust $ mkPoly [ p2 0.0 0.0
                                       , p2 0.0 16.0
                                       , p2 3.0 8.0
                                       , p2 8.0 8.0
                                       , p2 8.0 5.0
                                       , p2 8.0 0.0 ]
          clipPoly = square 8.0 (p2 0.0 8.0)
          intersection = [p2 0.0 8.0, p2 0.0 16.0, p2 3.0 8.0]
       in case sutherlandHodgman inside clipPoly subjPoly of
            Nothing -> failure "failed to find an intersection"
            Just resultPoly -> do
              let result = points resultPoly
              Assert.equal [] (result \\ intersection)
              Assert.equal [] (intersection \\ result)

    test "no intersection"
      let subjPoly = fromJust $ mkPoly [ p2 0.0 0.0
                                       , p2 0.0 16.0
                                       , p2 3.0 8.0
                                       , p2 8.0 8.0
                                       , p2 8.0 5.0
                                       , p2 8.0 0.0 ]
          clipPoly = square 8.0 (p2 8.0 0.0)
          resultPoly = sutherlandHodgman inside clipPoly subjPoly
       in Assert.assert "no intersection" (isNothing resultPoly)

  suite "prunePoly" do

    test "no excess points"
      let ps = [p2 0.0 0.0, p2 0.0 8.0, p2 8.0 8.0, p2 8.0 0.0]
          poly = fromJust $ mkPoly ps
       in case prunePoly poly of
            Nothing -> failure "removed too many points"
            Just poly' -> Assert.equal ps (points poly')

    test "one excess point"
      let ps = [ p2 0.0 0.0
               , p2 0.0 8.0
               , p2 8.0 8.0
               , p2 8.0 0.0]
          poly = fromJust $ mkPoly [ p2 0.0 0.0
                                   , p2 0.0 8.0
                                   , p2 4.0 8.0 -- excess point
                                   , p2 8.0 8.0
                                   , p2 8.0 0.0]
       in case prunePoly poly of
            Nothing -> failure "removed too many points"
            Just poly' -> Assert.equal ps (points poly')

    test "two excess points, same edge"
      let ps = [ p2 0.0 0.0
               , p2 0.0 8.0
               , p2 8.0 8.0
               , p2 8.0 0.0]
          poly = fromJust $ mkPoly [ p2 0.0 0.0
                                   , p2 0.0 8.0
                                   , p2 4.0 8.0 -- excess point
                                   , p2 5.0 8.0 -- excess point
                                   , p2 8.0 8.0
                                   , p2 8.0 0.0]
       in case prunePoly poly of
            Nothing -> failure "removed too many points"
            Just poly' -> Assert.equal ps (points poly')

    test "two excess points, different edges"
      let ps = [ p2 0.0 0.0
               , p2 0.0 8.0
               , p2 8.0 8.0
               , p2 8.0 0.0]
          poly = fromJust $ mkPoly [ p2 0.0 0.0
                                   , p2 0.0 4.0 -- excess point
                                   , p2 0.0 8.0
                                   , p2 8.0 8.0
                                   , p2 8.0 4.0 -- excess point
                                   , p2 8.0 0.0]
       in case prunePoly poly of
            Nothing -> failure "removed too many points"
            Just poly' -> Assert.equal ps (points poly')

  suite "isConvex" do

    test "convex"
      let poly = fromJust $ mkPoly [ p2 0.0 0.0
                                   , p2 0.0 8.0
                                   , p2 8.0 8.0
                                   , p2 8.0 0.0 ]
       in Assert.assert "convex" (isConvex poly)

    test "concave"
      let poly = fromJust $ mkPoly [ p2 0.0 0.0
                                   , p2 0.0 16.0
                                   , p2 3.0 8.0
                                   , p2 8.0 8.0
                                   , p2 8.0 0.0 ]
       in Assert.assertFalse "concave" (isConvex poly)
