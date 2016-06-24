module Data.Parse.JSONSpec (main, spec) where

import Test.Hspec

import Data.Parse.JSON
import Text.ParserCombinators.Parsec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "boolTrue" $ do
    it "parses a true value" $ do
      parse boolTrue "" "true" `shouldBe` Right True
    it "doesn't parse a capitalized True value" $ do
      parse boolTrue "" "True" `shouldSatisfy` isLeft

  describe  "boolFalse" $ do
    it "parses a false value" $ do
      parse boolFalse "" "false" `shouldBe` Right False
    it "doesn't parse a capitalized False value" $ do
      parse boolFalse "" "False" `shouldSatisfy` isLeft

  describe "boolJSON" $ do
    it "parses a true value" $ do
      parse boolJSON "" "true" `shouldBe` Right (JSONBool True)
    it "parses a false value" $ do
      parse boolJSON "" "false" `shouldBe` Right (JSONBool False)
    it "doesn't parse a non-boolean value" $ do
      parse boolJSON "" "non-bool" `shouldSatisfy` isLeft

  describe "numString" $ do
    it "parses a positive integer" $ do
      parse numString "" "12345" `shouldBe` Right "12345"
    it "parses a negative integer" $ do
      parse numString "" "-1234" `shouldBe` Right "-1234"
    it "parses a decimal value" $ do
      parse numString "" "89.123" `shouldBe` Right "89.123"
    it "parses a number in scientific notation" $ do
      parse numString "" "1.234e14" `shouldBe` Right "1.234e14"
    it "parses a number in scientific notation with capital E" $ do
      parse numString "" "1.23E4" `shouldBe` Right "1.23E4"
    it "doesn't parse a non-number value" $ do
      parse numString "" "true" `shouldSatisfy` isLeft

  describe "numJSON" $ do
    it "parses a positive integer" $ do
      parse numJSON "" "12345" `shouldBe` Right (JSONNum 12345.0)
    it "parses a negaive integer" $ do
      parse numJSON "" "-1234" `shouldBe` Right (JSONNum (-1234.0))
    it "parses a decimal value" $ do
      parse numJSON "" "89.123" `shouldBe` Right (JSONNum 89.123)
    it "parses a number in scientific notation" $ do
      parse numJSON "" "1.234e14" `shouldBe` Right (JSONNum 1.234e14)
    it "parses a number in scientific notation with capital E" $ do
      parse numJSON "" "1.23E4" `shouldBe` Right (JSONNum 1.23E4)
    it "doesn't parse a non-number value" $ do
      parse numJSON "" "true" `shouldSatisfy` isLeft

  describe "stringJSON" $ do
    it "parses a normal string" $ do
      parse stringJSON "" "\"string\"" `shouldBe` Right (JSONString "string")
    it "doesn't parse a non-string value" $ do
      parse stringJSON "" "non-string" `shouldSatisfy` isLeft

  describe "nullJSON" $ do
    it "parses a null value" $ do
      parse nullJSON "" "null" `shouldBe` Right JSONNull
    it "doesn't parse a non-null value" $ do
      parse nullJSON "" "non-null" `shouldSatisfy` isLeft

  describe "arrayJSON" $ do
    it "parses a one element list" $ do
      parse arrayJSON "" "[1]" `shouldBe` Right (JSONArray [JSONNum 1.0])
    it "parses a multi-element list" $ do
      parse arrayJSON "" "[1,2]" `shouldBe` Right (JSONArray [JSONNum 1.0, JSONNum 2.0])
    it "parses an empty list" $ do
      parse arrayJSON "" "[]" `shouldBe` Right (JSONArray [])
    it "parses a list of different elements" $ do
      parse arrayJSON "" "[1,true,\"string\"]" `shouldBe` Right (JSONArray [JSONNum 1.0, JSONBool True, JSONString "string"])
    it "parses a list containing a list" $ do
      parse arrayJSON "" "[[1],2]" `shouldBe` Right (JSONArray [JSONArray [JSONNum 1.0], JSONNum 2.0])
    it "parses a list with separating spaces" $ do
      parse arrayJSON "" "[1, 2]" `shouldBe` Right (JSONArray [JSONNum 1.0, JSONNum 2.0])
    it "parses a list with spaces before first element" $ do
      parse arrayJSON "" "[ 1]" `shouldBe` Right (JSONArray [JSONNum 1.0])
    it "parses an empty list with spaces" $ do
      parse arrayJSON "" "[   ]" `shouldBe` Right (JSONArray [])
    it "doesn't parse a non-comma seperated list" $ do
      parse arrayJSON "" "[1true]" `shouldSatisfy` isLeft
    it "doesn't parse a list without brackets" $ do
      parse arrayJSON "" "true,false" `shouldSatisfy` isLeft
    it "doesn't parse a list with unmatched brackets" $ do
      parse arrayJSON "" "[true,fale" `shouldSatisfy` isLeft
    it "doesn't parse a list with an empty first value" $ do
      parse arrayJSON "" "[,true]" `shouldSatisfy` isLeft

  describe "objectJSON" $ do
    it "parses a single field object" $ do
      parse objectJSON "" "{\"a\":true}" `shouldBe` Right (JSONObject [("a", JSONBool True)])
    it "parses a multi field object" $ do
      parse objectJSON "" "{\"a\":true,\"b\":false}" `shouldBe` Right (JSONObject [("a", JSONBool True), ("b", JSONBool False)])
    it "parses an object with no fields" $ do
      parse objectJSON "" "{}" `shouldBe` Right (JSONObject [])
    it "parses an object with separating spaces" $ do
      parse objectJSON "" "{ \"a\" : true , \"b\" : 123 }" `shouldBe` Right (JSONObject [("a", JSONBool True), ("b", JSONNum 123.0)])
    it "parses an object with newline sperated fields" $ do
      parse objectJSON "" "{\n\"a\":true\n}" `shouldBe` Right (JSONObject [("a", JSONBool True)])
    it "doesn't parse an invalid object" $ do
      parse objectJSON "" "}\"s\":false{" `shouldSatisfy` isLeft

  describe "valueJSON" $ do
    it "parses a boolean value" $ do
      parse valueJSON "" "true" `shouldBe` Right (JSONBool True)
    it "parses a number value" $ do
      parse valueJSON "" "12345" `shouldBe` Right (JSONNum 12345)
    it "parses a string" $ do
      parse valueJSON "" "\"string\"" `shouldBe` Right (JSONString "string")
    it "parses a null value" $ do
      parse valueJSON "" "null" `shouldBe` Right JSONNull
    it "parses an array" $ do
      parse valueJSON "" "[1,2]" `shouldBe` Right (JSONArray [JSONNum 1.0, JSONNum 2.0])
    it "parses an object" $ do
      parse valueJSON "" "{\"a\":true}" `shouldBe` Right (JSONObject [("a", JSONBool True)])
    it "doesn't parse a non-JSON value" $ do
      parse valueJSON "" "{{]]f343}[[;" `shouldSatisfy` isLeft

-- | Tells if the given Either is a Left.
isLeft :: Either a b -> Bool
isLeft e = case e of
  Left _  -> True
  Right _ -> False
