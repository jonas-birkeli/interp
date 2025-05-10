module Interp.Parse.Token(
    parseControl,
    parseDefinition,
    parseOperator,
    parseList,
    parseQuotation,
    parseToken,
    parseProgram
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Parse.Value

-- | Parse a control operation
--
-- >>> parseControl "if"
-- Right If
--
-- >>> parseControl "times"
-- Right Times
--
-- >>> parseControl "xyz"
-- Left (ParseError (UnknownToken "xyz"))
--
parseControl :: String -> Either InterpError ControlOp
parseControl "if" = Right If
parseControl "times" = Right Times
parseControl "loop" = Right Loop
parseControl "each" = Right Each
parseControl "map" = Right Map
parseControl "foldl" = Right Foldl
parseControl "exec" = Right Exec
parseControl s = Left . ParseError $ UnknownToken s

-- | Parse a definition operation
--
-- >>> parseDefinition ":="
-- Right Assign
--
-- >>> parseDefinition "fun"
-- Right FunDef
--
-- >>> parseDefinition "xyz"
-- Left (ParseError (UnknownToken "xyz"))
--
parseDefinition :: String -> Either InterpError DefOp
parseDefinition ":=" = Right Assign
parseDefinition "fun" = Right FunDef
parseDefinition s = Left . ParseError $ UnknownToken s

-- | Parse an operator
--
-- >>> parseOperator "+"
-- Right "+"
--
-- >>> parseOperator "pop"
-- Right "pop"
--
parseOperator :: String -> Either InterpError String
parseOperator s | s `elem` operators = Right s
                | otherwise = Left . ParseError $ UnknownToken s
  where
    operators = [ "+", "-", "*", "/", "div"
                , "<", ">", "=="
                , "&&", "||", "not"
                , "dup", "swap", "pop"
                , "print", "read"
                , "parseInteger", "parseFloat", "words"
                , "head", "tail", "empty", "length", "cons", "append"
                ]

-- | Parse a list from tokens
--
-- >>> parseList ["[", "1", "2", "3", "]", "rest"]
-- Right (ListValue [IntValue 1,IntValue 2,IntValue 3],["rest"])
--
-- >>> parseList ["[", "]", "rest"]
-- Right (ListValue [],["rest"])
--
-- >>> parseList ["[", "1", "2"]
-- Left (ParseError (IncompleteList "["))
--
parseList :: [String] -> Either InterpError (Value, [String])
parseList [] = Left . ParseError $ IncompleteList "["
parseList ("[":toks) = go [] toks
  where
    go acc [] = Left . ParseError $ IncompleteList "["
    go acc ("]":rest) = Right (ListValue (reverse acc), rest)
    go acc ("[":rest) = parseList ("[":rest) >>= \(nested, rest') ->
        go (nested : acc) rest'
    go acc ("{":rest) = parseQuotation ("{":rest) >>= \(QuotationValue ts, rest') ->
        go (QuotationValue ts : acc) rest'
    go acc (t:rest) = parseValue t >>= \v -> go (v : acc) rest
parseList _ = Left . ParseError $ UnknownToken "Expected ["

-- | Parse a quotation from tokens
--
-- >>> parseQuotation ["{", "1", "2", "+", "}", "rest"]
-- Right (QuotationValue [LiteralToken (IntValue 1),LiteralToken (IntValue 2),OperatorToken "+"],["rest"])
--
-- >>> parseQuotation ["{", "}", "rest"]
-- Right (QuotationValue [],["rest"])
--
-- >>> parseQuotation ["{", "1", "2"]
-- Left (ParseError (IncompleteQuotation "{"))
--
parseQuotation :: [String] -> Either InterpError (Value, [String])
parseQuotation [] = Left . ParseError $ IncompleteQuotation "{"
parseQuotation ("{":toks) = go [] toks
  where
    go _ [] = Left . ParseError $ IncompleteQuotation "{"
    go acc ("}":rest) = Right (QuotationValue (reverse acc), rest)
    go acc ts = parseToken ts >>= \(tok, rest) -> go (tok : acc) rest
parseQuotation _ = Left . ParseError $ UnknownToken "Expected {"

-- | Parse a single token
--
-- >>> parseToken ["42", "rest"]
-- Right (LiteralToken (IntValue 42),["rest"])
--
-- >>> parseToken ["+", "rest"]
-- Right (OperatorToken "+",["rest"])
--
-- >>> parseToken ["if", "rest"]
-- Right (ControlToken If,["rest"])
--
-- >>> parseToken [":=", "rest"]
-- Right (DefinitionToken Assign,["rest"])
--
-- >>> parseToken ["{", "1", "+", "}", "rest"]
-- Right (LiteralToken (QuotationValue [LiteralToken (IntValue 1),OperatorToken "+"]),["rest"])
--
-- >>> parseToken ["[", "1", "2", "]", "rest"]
-- Right (LiteralToken (ListValue [IntValue 1,IntValue 2]),["rest"])
--
parseToken :: [String] -> Either InterpError (Token, [String])
parseToken [] = Left . ParseError $ UnknownToken ""
parseToken (tok:rest)
    | tok == "{" = parseQuotation (tok:rest) >>= \(v, rest') ->
        Right (LiteralToken v, rest')
    | tok == "[" = parseList (tok:rest) >>= \(v, rest') ->
        Right (LiteralToken v, rest')
    | otherwise = 
        case parseControl tok of
            Right ctrl -> Right (ControlToken ctrl, rest)
            Left _ -> case parseDefinition tok of
                Right def -> Right (DefinitionToken def, rest)
                Left _ -> case parseOperator tok of
                    Right op -> Right (OperatorToken op, rest)
                    Left _ -> parseValue tok >>= \v ->
                        Right (LiteralToken v, rest)

-- | Parse a program from tokens
--
-- >>> parseProgram ["1", "2", "+"]
-- Right [LiteralToken (IntValue 1),LiteralToken (IntValue 2),OperatorToken "+"]
--
-- >>> parseProgram ["True", "if", "{", "1", "}", "{", "2", "}"]
-- Right [LiteralToken (BoolValue True),ControlToken If,LiteralToken (QuotationValue [LiteralToken (IntValue 1)]),LiteralToken (QuotationValue [LiteralToken (IntValue 2)])]
--
parseProgram :: [String] -> Either InterpError Program
parseProgram [] = Right []
parseProgram toks = parseToken toks >>= \(t, rest) ->
    parseProgram rest >>= \prog -> Right (t : prog)