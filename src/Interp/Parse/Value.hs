module Interp.Parse.Value(
    parseString,
    parseNumeric,
    parseBoolean,
    parseValue
) where

import Interp.Core.Types
import Interp.Core.Error
import Text.Read (readMaybe)
import Data.Char (isDigit)

-- | Parse a string value, removing quotes and trimming whitespace
--
-- >>> parseString "\" hello world \""
-- Right (StringValue "hello world")
--
-- >>> parseString "\" \""
-- Right (StringValue "")
--
-- >>> parseString "\""
-- Left (ParseError (IncompleteString "\""))
--
parseString :: String -> Either InterpError Value
parseString s = case s of
    ('"':rest) -> case reverse rest of
        ('"':rrest) -> Right . StringValue . trim . reverse $ rrest
        _ -> Left . ParseError $ IncompleteString s
    _ -> Left . ParseError $ UnknownToken s
  where
    trim :: String -> String
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    isSpace :: Char -> Bool
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- | Parse a numeric value (int or float)
--
-- >>> parseNumeric "42"
-- Right (IntValue 42)
--
-- >>> parseNumeric "3.14"
-- Right (FloatValue 3.14)
--
-- >>> parseNumeric "-42"
-- Right (IntValue (-42))
--
-- >>> parseNumeric "-3.14"
-- Right (FloatValue (-3.14))
--
-- >>> parseNumeric "abc"
-- Left (ParseError (UnknownToken "abc"))
--
parseNumeric :: String -> Either InterpError Value
parseNumeric s
    | isInteger s = case readMaybe s of
        Just i -> Right $ IntValue i
        Nothing -> Left . ParseError $ UnknownToken s
    | isFloat s = case readMaybe s of
        Just f -> Right $ FloatValue f
        Nothing -> Left . ParseError $ UnknownToken s
    | otherwise = Left . ParseError $ UnknownToken s
  where
    isInteger :: String -> Bool
    isInteger ('-':xs) = not (null xs) && all isDigit xs
    isInteger xs = not (null xs) && all isDigit xs
    
    isFloat :: String -> Bool
    isFloat ('-':xs) = isValidFloat xs
    isFloat xs = isValidFloat xs
    
    isValidFloat :: String -> Bool
    isValidFloat i = case break (== '.') i of
        (before, '.':after) -> 
            not (null before) && all isDigit before &&
            not (null after) && all isDigit after
        _ -> False

-- | Parse a boolean value
--
-- >>> parseBoolean "True"
-- Right (BoolValue True)
--
-- >>> parseBoolean "False"
-- Right (BoolValue False)
--
-- >>> parseBoolean "true"
-- Left (ParseError (UnknownToken "true"))
--
parseBoolean :: String -> Either InterpError Value
parseBoolean "True" = Right $ BoolValue True
parseBoolean "False" = Right $ BoolValue False
parseBoolean s = Left . ParseError $ UnknownToken s

-- | Parse any value
--
-- >>> parseValue "42"
-- Right (IntValue 42)
--
-- >>> parseValue "True"
-- Right (BoolValue True)
--
-- >>> parseValue "\" hello \""
-- Right (StringValue "hello")
--
-- >>> parseValue "symbol"
-- Right (SymbolValue "symbol")
--
parseValue :: String -> Either InterpError Value
parseValue "" = Left . ParseError $ UnknownToken ""
parseValue s@(c:_)
    | c == '"' = parseString s
    | s `elem` ["True", "False"] = parseBoolean s
    | isNumeric s = parseNumeric s
    | otherwise = Right $ SymbolValue s
  where
    isNumeric :: String -> Bool
    isNumeric str = case parseNumeric str of
        Right _ -> True
        Left _ -> False