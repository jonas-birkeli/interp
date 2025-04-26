module Parser where

import Types

-- | Parse a program string into a list of tokens
parseProgram :: String -> Either ParseError [Token]
parseProgram input = parseTokens $ words input

-- | Parse a list of words into tokens
parseTokens :: [String] -> Either ParseError [Token]
parseTokens [] = Right []
parseTokens (w:ws)
    | w == "{" do
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

-- | Parse a single token
parseToken :: String -> Either ParserError Token