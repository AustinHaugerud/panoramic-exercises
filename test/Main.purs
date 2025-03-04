module Test.Main where

import Prelude

import Effect (Effect)
import Test.Exercise1.App.Data.BoundedIntSpec (spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = do
  runSpecAndExitProcess [consoleReporter] do
    spec
