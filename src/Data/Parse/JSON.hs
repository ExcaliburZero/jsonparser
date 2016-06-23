-----------------------------------------------------------------------------
-- |
-- Module      : Data.Parse.JSON
-- Description : Contains parsers for parsing JSON
-- Copyright   : (c) Christopher Wells, 2016
-- License     : MIT
-- Maintainer  : cwellsny@nycap.rr.com
-----------------------------------------------------------------------------

module Data.Parse.JSON where

import Control.Applicative ((<$>), (<|>), (*>), (<*), many)
import Text.ParserCombinators.Parsec (char, noneOf, parse, Parser, string)

-- | Represents a JSON value.
data JSONValue =
    JSONBool Bool                     -- ^ A boolean value
  | JSONInt Int                       -- ^ A number
  | JSONString String                 -- ^ A string
  | JSONObject [(String, JSONValue)]  -- ^ A list of key value pairs
  | JSONArray [JSONValue]             -- ^ An array of JSON values
  | JSONNull                          -- ^ A null value
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

-- | Parses a JSON string value.
--
-- >>> parse stringJSON "" "\"string\""
-- Right (JSONString "string")
stringJSON :: Parser JSONValue
stringJSON = JSONString <$> (char '\"' *> (many $ noneOf "\"") <* char '\"')

-- | Parses a JSON null value.
--
-- >>> parse nullJSON "" "null"
-- Right JSONNull
nullJSON :: Parser JSONValue
nullJSON = (const JSONNull) <$> string "null"

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
valueJSON = boolJSON <|> stringJSON <|> nullJSON
