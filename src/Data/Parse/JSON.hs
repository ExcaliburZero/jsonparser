-----------------------------------------------------------------------------
-- |
-- Module      : Data.Parse.JSON
-- Description : Contains parsers for parsing JSON
-- Copyright   : (c) Christopher Wells, 2016
-- License     : MIT
-- Maintainer  : cwellsny@nycap.rr.com
-----------------------------------------------------------------------------

module Data.Parse.JSON where

import Control.Applicative ((<$>), (<|>), (<*>), (*>), (<*), empty, many)
import Text.ParserCombinators.Parsec.Token ()
import Text.ParserCombinators.Parsec (char, digit, noneOf, oneOf, parse, Parser, spaces, string)

-- | Represents a JSON value.
data JSONValue =
    JSONBool Bool                     -- ^ A boolean value
  | JSONNum Float                     -- ^ A number
  | JSONString String                 -- ^ A string
  | JSONNull                          -- ^ A null value
  | JSONArray [JSONValue]             -- ^ An array of JSON values
  | JSONObject [(String, JSONValue)]  -- ^ A list of key value pairs
  deriving (Eq, Show)

-- | Parses a "true" string into a True value.
--
-- >>> parse boolTrue "" "true"
-- Right True
boolTrue :: Parser Bool
boolTrue = fmap (const True) $ string "true"

-- | Parses a "false" string into a False value.
--
-- >>> parse boolFalse "" "false"
-- Right False
boolFalse :: Parser Bool
boolFalse = fmap (const False) $ string "false"

-- | Parses a JSON boolean value.
--
-- >>> parse boolJSON "" "true"
-- Right (JSONBool True)
-- >>> parse boolJSON "" "false"
-- Right (JSONBool False)
boolJSON :: Parser JSONValue
boolJSON = fmap (JSONBool) $ boolTrue <|> boolFalse

-- | Parses a number into a String.
--
-- >>> parse numString "" "12345"
-- Right "12345"
--
-- >>> parse numString "" "1.234e5"
-- Right "1.234e5"
numString :: Parser String
numString = (++) <$> firstChar <*> (many (digit <|> oneOf "Ee."))
  where firstChar    = charToString <$> (digit <|> oneOf "-")
        charToString = \x -> [x]

-- | Parses a JSON number value
--
-- >>> parse numJSON "" "12345"
-- Right (JSONNum 12345.0)
--
-- >>> parse numJSON "" "1.234e5"
-- Right (JSONNum 123400.0)
numJSON :: Parser JSONValue
numJSON = JSONNum <$> (read :: String -> Float) <$> numString

-- | Parses a JSON string value into a regular String.
--
-- >>> parse stringParser "" "\"string\""
-- Right "string"
stringParser :: Parser String
stringParser = char '\"' *> (many $ noneOf "\"") <* char '\"'

-- | Parses a JSON string value.
--
-- >>> parse stringJSON "" "\"string\""
-- Right (JSONString "string")
stringJSON :: Parser JSONValue
stringJSON = JSONString <$> stringParser

-- | Parses a JSON null value.
--
-- >>> parse nullJSON "" "null"
-- Right JSONNull
nullJSON :: Parser JSONValue
nullJSON = (const JSONNull) <$> string "null"

-- | Parses an array of JSON values.
--
-- >>> parse arrayJSON "" "[1, 2]"
-- Right (JSONArray [JSONNum 1.0,JSONNum 2.0])
--
-- >>> parse arrayJSON "" "[true, null]"
-- Right (JSONArray [JSONBool True,JSONNull])
--
-- >>> parse arrayJSON "" "[]"
-- Right (JSONArray [])
arrayJSON :: Parser JSONValue
arrayJSON = JSONArray <$> (char '[' *> spaces *> valuesAndEnding)
  where valuesAndEnding = withValues <|> withoutValues
        withValues    = values <* char ']'
        withoutValues = (const []) <$> char ']'
        values = (:) <$> valueJSON <*> (many $ (char ',' *> spaces *> valueJSON))

-- | Parses a JSON object.
--
-- >>> parse objectJSON "" "{\"a\":true}"
-- Right (JSONObject [("a",JSONBool True)])
objectJSON :: Parser JSONValue
objectJSON = JSONObject <$> (start *> pairsAndEnding)
  where start = char '{' <* spaces
        pairsAndEnding = withPairs <|> withoutPairs
        withoutPairs = (const []) <$> char '}'
        withPairs = pairs <* char '}'
        pairs = (:) <$> pair <*> (many $ (char ',' *> spaces *> pair))
        pair = do
          key <- stringParser
          spaces
          _ <- char ':'
          spaces
          value <- valueJSON
          spaces
          return (key, value)

-- | Parses a JSON value.
--
-- >>> parse valueJSON "" "true"
-- Right (JSONBool True)
--
-- >>> parse valueJSON "" "\"string\""
-- Right (JSONString "string")
--
-- >>> parse valueJSON "" "null"
-- Right JSONNull
valueJSON :: Parser JSONValue
valueJSON = boolJSON <|> numJSON <|> stringJSON <|> nullJSON <|> arrayJSON <|> objectJSON
