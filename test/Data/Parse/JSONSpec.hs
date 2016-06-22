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

  describe "stringJSON" $ do
    it "parses a normal string" $ do
      parse stringJSON "" "\"string\"" `shouldBe` Right (JSONString "string")
    it "doesn't parse a non-string value" $ do
      parse stringJSON "" "non-string" `shouldSatisfy` isLeft

  describe "valueJSON" $ do
    it "parses a boolean value" $ do
      parse valueJSON "" "true" `shouldBe` Right (JSONBool True)
    it "parses a string" $ do
      parse valueJSON "" "\"string\"" `shouldBe` Right (JSONString "string")
    it "doesn't parse a non-JSON value" $ do
      parse valueJSON "" "{{]]f343}[[;" `shouldSatisfy` isLeft

-- | Tells if the given Either is a Left.
isLeft :: Either a b -> Bool
isLeft e = case e of
  Left _  -> True
  Right _ -> False
