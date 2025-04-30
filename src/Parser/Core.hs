module Parser.Core
    (
        parseProgram,
        parseTokens,
        parseQuotation,
        parseList
    ) where

import Types (ParseError(..), Token(..), Value(..))
import Parser.Tokenizer (tokenize)
import Parser.Value (parseValue, parseToken)

-- | Parse a program string into a list of tokens
parseProgram :: String -> Either ParseError [Token]
parseProgram input = parseTokens $ tokenize input

-- | Parse a list of words into tokens
parseTokens :: [String] -> Either ParseError [Token]
parseTokens [] = Right []
parseTokens (w:ws)
    | w == "{" = do
        (quotation, rest) <- parseQuotation ws
        tokens <- parseTokens rest -- Recursive
        return $ ValueToken (QuotationValue quotation) : tokens
    | w == "[" = do
        (list, rest) <- parseList ws
        tokens <- parseTokens rest -- Recursive
        return $ ValueToken (ListValue list) : tokens
    | otherwise = do
        token <- parseToken w
        tokens <- parseTokens ws
        return $ token : tokens

-- | Parse a quotation 
parseQuotation :: [String] -> Either ParseError ([Token], [String])
parseQuotation [] = Left IncompleteQuotation
parseQuotation (w:ws)
    | w == "}" = Right ([], ws)
    | w == "{" = do
        (nestedQuotation, rest1) <- parseQuotation ws
        (quotationTail, rest2) <- parseQuotation rest1
        return (ValueToken (QuotationValue nestedQuotation) : quotationTail, rest2)
    | w == "[" = do
        (list, rest1) <- parseList ws
        (quotationTail, rest2) <- parseQuotation rest1
        return (ValueToken (ListValue list) : quotationTail, rest2)
    | otherwise = do
        token <- parseToken w
        (quotationTail, rest) <- parseQuotation ws
        return (token : quotationTail, rest)

-- | Parse a list
parseList :: [String] -> Either ParseError ([Value], [String])
parseList [] = Left IncompleteList
parseList (w:ws)
    | w == "]" = Right ([], ws)
    | w == "[" = do
        (nestedList, rest1) <- parseList ws
        (listTail, rest2) <- parseList rest1
        return (ListValue nestedList : listTail, rest2)
    | w == "{" = do
        (quotation, rest1) <- parseQuotation ws
        (listTail, rest2) <- parseList rest1
        return (QuotationValue quotation : listTail, rest2)
    | otherwise = do
        value <- parseValue w
        (listTail, rest) <- parseList ws
        return (value : listTail, rest)