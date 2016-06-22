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

-- | Tells if the given Either is a Left.
isLeft :: Either a b -> Bool
isLeft e = case e of
  Left _  -> True
  Right _ -> False
