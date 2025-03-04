module Test.Exercise1.App.Data.BoundedIntSpec where

import Prelude

import Exercise1.App.Data.BoundedInt (boundedInt, toInt, fromInt)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Exercise1.App.Data.BoundedInt" do
    it "accepts static values" do
      let val = boundedInt @0 @10 @5
      toInt val `shouldEqual` 5

    it "accepts static bottom inclusive values" do
      let val = boundedInt @0 @10 @0
      toInt val `shouldEqual` 0
    
    it "accepts static top inclusive values" do
      let val = boundedInt @0 @10 @10
      toInt val `shouldEqual` 10

    it "accepts runtime valid values" do
      fromInt @0 @10 5 `shouldEqual` Just (boundedInt @0 @10 @5)

    it "accepts runtime bottom inclusive values" do
      fromInt @0 @10 0 `shouldEqual` Just (boundedInt @0 @10 @0)

    it "accepts runtime upper inclusive values" do
      fromInt @0 @10 10 `shouldEqual` Just (boundedInt @0 @10 @10)

    it "rejects runtime low values" do
      fromInt @0 @10 (-1) `shouldEqual` Nothing

    it "rejects runtime high values" do
      fromInt @0 @10 11 `shouldEqual` Nothing

    it "prints a valid expression for `show`" do
      let
        staticPrintout = show (boundedInt @0 @10 @5)
        dynamicPrintout = show <$> (fromInt @0 @10 5)
        expected = "boundedInt @0 @10 @5"

      staticPrintout `shouldEqual` expected
      dynamicPrintout `shouldEqual` Just expected
