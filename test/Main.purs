module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)

import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)

import Test.Data (dataSuite)

--main :: forall a. Eff (assert :: ASSERT | a) Unit
main :: forall e. Eff ( console :: CONSOLE
                      , testOutput :: TESTOUTPUT
                      , avar :: AVAR
                      | e )
                      Unit
main = runTest do
  dataSuite
