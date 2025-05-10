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
-- Right ["{ 1 2 }","+"]
--
-- >>> splitPreserveTokens "[ a b c ] d"
-- Right ["[ a b c ]","d"]
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
splitPreserveTokens = fmap reverse . finalize . foldl collect ([], Nothing) . words
  where
    collect :: ([String], Maybe (String, [String])) -> String -> ([String], Maybe (String, [String]))
    collect (acc, Nothing) tok
      | tok == "{"  = (acc, Just ("}", [tok]))
      | tok == "["  = (acc, Just ("]", [tok]))
      | tok == "\"" = (acc, Just ("\"", [tok]))
      | otherwise   = (tok : acc, Nothing)
    
    collect (acc, Just (end, group)) tok
      | tok == end = (unwords (reverse (tok : group)) : acc, Nothing)
      | otherwise  = (acc, Just (end, tok : group))
    
    finalize :: ([String], Maybe (String, [String])) -> Either InterpError [String]
    finalize (acc, Nothing) = Right acc
    finalize (_, Just (end, group)) = case end of
      "}"  -> Left . ParseError $ IncompleteQuotation (unwords (reverse group))
      "]"  -> Left . ParseError $ IncompleteList (unwords (reverse group))
      "\"" -> Left . ParseError $ IncompleteString (unwords (reverse group))
      _    -> Left . ParseError $ UnexpectedEnd (fromMaybe '?' (listToMaybe end))