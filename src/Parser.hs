module Parser
    (  
        parseProgram,
    ) where

import Types (ParseError(..), Token(..), Value(..))
import Tokenizer (tokenize)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Control.Monad (guard)



-- | Parse a single value
parseValue :: String -> Either ParseError Value
parseValue "True" = Right $ BoolValue True
parseValue "False" = Right $ BoolValue False
parseValue s@('"':rest) =
    if not (null rest) && last rest == '"'
        then Right $ StringValue $ trimString $ init rest
        else Left IncompleteString
parseValue w
    | all isDigit w = Right $ IntValue $ read w
    | all (\c -> isDigit c || c == '.' || c == '-') w && (w /= "-") && '.' `elem` w =
        case readMaybe w of
            Just n -> Right $ FloatValue n
            Nothing -> Left IncompleteString
    | all (\c -> isDigit c || c == '-') w && (w /= "-") =
        case readMaybe w of
            Just n -> Right $ IntValue n
            Nothing -> Left $ UnknownToken w
    | otherwise = Right $ SymbolValue w

-- | Parse a single token, wrap in OperatorToken
parseToken :: String -> Either ParseError Token
parseToken "+" = Right $ OperatorToken "+"
parseToken "-" = Right $ OperatorToken "-"
parseToken "*" = Right $ OperatorToken "*"
parseToken "/" = Right $ OperatorToken "/"
parseToken "div" = Right $ OperatorToken "div"
parseToken "<" = Right $ OperatorToken "<"
parseToken ">" = Right $ OperatorToken ">"
parseToken "==" = Right $ OperatorToken "=="
parseToken "&&" = Right $ OperatorToken "&&"
parseToken "||" = Right $ OperatorToken "||"
parseToken "not" = Right $ OperatorToken "not"
parseToken "dup" = Right $ OperatorToken "dup"
parseToken "swap" = Right $ OperatorToken "swap"
parseToken "pop" = Right $ OperatorToken "pop"
parseToken "print" = Right $ OperatorToken "print"
parseToken "read" = Right $ OperatorToken "read"
parseToken "parseInteger" = Right $ OperatorToken "parseInteger"
parseToken "parseFloat" = Right $ OperatorToken "parseFloat"
parseToken "words" = Right $ OperatorToken "words"
parseToken "head" = Right $ OperatorToken "head"
parseToken "tail" = Right $ OperatorToken "tail"
parseToken "empty" = Right $ OperatorToken "empty"
parseToken "length" = Right $ OperatorToken "length"
parseToken "cons" = Right $ OperatorToken "cons"
parseToken "append" = Right $ OperatorToken "append"
parseToken "if" = Right IfToken 
parseToken "times" = Right TimesToken
parseToken "loop" = Right LoopToken
parseToken "each" = Right EachToken
parseToken "map" = Right MapToken
parseToken "foldl" = Right FoldlToken
parseToken "exec" = Right ExecToken
parseToken ":=" = Right AssignmentToken
parseToken "fun" = Right FunctionToken
parseToken "True" = Right $ ValueToken (BoolValue True)
parseToken "False" = Right $ ValueToken (BoolValue False)
parseToken s | isNegativeNumber s = parseNegativeNumber s
parseToken s | isFloat s = parseFloat s
parseToken s | isInteger s = parseInt s
parseToken s | isStringLiteral s = parseString s
parseToken "{" = Right $ ValueToken (QuotationValue [])  -- Placeholder handled by parseQuotation
parseToken "}" = Left UnbalancedBraces  -- Should not occur in isolation, handled by parseQuotation
parseToken "[" = Right $ ValueToken (ListValue [])  -- Placeholder handled by parseList
parseToken "]" = Left UnbalancedBrackets  -- Should not occur in isolation, handled by parseList
parseToken s = Right $ ValueToken (SymbolValue s)

-- | Check if a string is a negative number
isNegativeNumber :: String -> Bool
isNegativeNumber ('-':rest) = all isDigit rest || (all (\c -> isDigit c || c == '.') rest && '.' `elem` rest)
isNegativeNumber _ = False

-- | Parse a negative number
parseNegativeNumber :: String -> Either ParseError Token
parseNegativeNumber ('-':rest)
    | all isDigit rest = Right $ ValueToken $ IntValue $ negate $ read rest
    | all (\c -> isDigit c || c == '.') rest && '.' `elem` rest =
        case readMaybe ('-':rest) of
            Just n -> Right $ ValueToken $ FloatValue n
            Nothing -> Left $ UnknownToken ('-':rest)
parseNegativeNumber s = Left $ UnknownToken s

-- | Check if a string is an integer
isInteger :: String -> Bool
isInteger = all isDigit

-- | Parse an integer
parseInt :: String -> Either ParseError Token
parseInt s = Right $ ValueToken $ IntValue $ read s

-- | Check if a string is a float
isFloat :: String -> Bool
isFloat s = all (\c -> isDigit c || c == '.') s && '.' `elem` s

-- | Parse a float
parseFloat :: String -> Either ParseError Token
parseFloat s = case readMaybe s of
  Just n -> Right $ ValueToken $ FloatValue n
  Nothing -> Left $ UnknownToken s

-- | Check if a string is a string literal
isStringLiteral :: String -> Bool
isStringLiteral ('"':rest) = not (null rest) && last rest == '"'
isStringLiteral _ = False

-- | Parse a string literal
parseString :: String -> Either ParseError Token
parseString s@('"':rest) = 
    if not (null rest) && last rest == '"'
        then Right $ ValueToken $ StringValue $ trimString $ init rest
        else Left IncompleteString
parseString _ = Left IncompleteString

-- | Trim spaces from string
trimString :: String -> String
trimString = trimLeading . trimTrailing
    where
        trimLeading = dropWhile (== ' ')
        trimTrailing = reverse . trimLeading . reverse
