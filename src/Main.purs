module Main where

import Prelude

import Effect (Effect)
import Exercise2.App (app)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI app unit body
