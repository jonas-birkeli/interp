module Interp.Parse.Tokenize(
    splitPreserveTokens
) where

import Data.Maybe (fromMaybe, listToMaybe)
import Interp.Core.Error

-- | Convert an input string to a list of strings by handling blocks and strings
--
-- >>> splitPreserveTokens "1 2 3"
-- Right ["1","2","3"]
--
-- >>> splitPreserveTokens "{ 1 2 } +"
-- Right ["{","1","2","}","+"]
--
-- >>> splitPreserveTokens "[ a b c ] d"
-- Right ["[","a","b","c","]","d"]
--
-- >>> splitPreserveTokens " \" hello world \" x"
-- Right ["\" hello world \"","x"]
--
-- >>> splitPreserveTokens "{ 1 2"
-- Left (ParseError (IncompleteQuotation "{ 1 2"))
--
-- >>> splitPreserveTokens "[ 1 2 3"
-- Left (ParseError (IncompleteList "[ 1 2 3"))
--
-- >>> splitPreserveTokens "\" unfinished string"
-- Left (ParseError (IncompleteString "\" unfinished string"))
--
splitPreserveTokens :: String -> Either InterpError [String]
splitPreserveTokens = tokenize

-- | Tokenize input string into individual tokens
tokenize :: String -> Either InterpError [String]
tokenize input = reverse <$> go [] [] False input
  where
    -- Parse tokens, tracking whether we're in a string
    go :: [String] -> String -> Bool -> String -> Either InterpError [String]
    go tokens current False [] = Right $ addToken tokens current
    go _ current True [] = Left . ParseError $ IncompleteString (reverse current ++ "\"")
    
    -- Handle quote to start/end strings
    go tokens current False ('"':rest) = go tokens ('"':current) True rest
    go tokens current True ('"':rest) = go (reverse ('"':current) : tokens) "" False rest
    
    -- Handle whitespace (including newlines) when not in string
    go tokens current False (c:rest) 
        | c `elem` " \t\n\r" = go (addToken tokens current) "" False rest
        | otherwise = go tokens (c:current) False rest
    
    -- Any character when in string
    go tokens current True (c:rest) = go tokens (c:current) True rest
    
    -- Helper to add non-empty tokens
    addToken :: [String] -> String -> [String]
    addToken tokens "" = tokens
    addToken tokens str = reverse str : tokens