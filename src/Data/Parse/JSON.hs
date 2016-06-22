-----------------------------------------------------------------------------
-- |
-- Module      : Data.Parse.JSON
-- Description : Contains parsers for parsing JSON
-- Copyright   : (c) Christopher Wells, 2016
-- License     : MIT
-- Maintainer  : cwellsny@nycap.rr.com
-----------------------------------------------------------------------------

module Data.Parse.JSON where

import Text.Parsec.Char (string)
import Text.ParserCombinators.Parsec ((<|>), Parser, parse)

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
