module Test.Linear where

import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert

import Linear.R2 (p2, v2, (.-.))

linearSuite :: forall a. TestSuite a
linearSuite = suite "Linear" do
  test "p2 sub" do
    Assert.equal (p2 2 2 .-. p2 1 1) (v2 1 1)
