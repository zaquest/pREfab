module Test.Data where

import Prelude
import Test.Unit (suite, TestSuite)
import Test.Data.Segment (segmentSuite)
import Test.Data.Corner (cornerSuite)
import Test.Data.BoundingBox (boundingBoxSuite)
import Test.Data.Polygon (polygonSuite)

dataSuite :: forall a. TestSuite a
dataSuite = suite "Data" do
  segmentSuite
  cornerSuite
  boundingBoxSuite
  polygonSuite
