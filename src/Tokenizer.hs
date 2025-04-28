module Tokenizer (tokenize) where

import Data.Char (isSpace)


tokenize :: String -> [String]
tokenize input = tokenize' [] "" False input
  where
    tokenize' acc curr inString [] 
        | null curr = reverse acc
        | otherwise = reverse (reverse curr : acc)
    
    tokenize' acc curr True ('"':cs) = 
        tokenize' (reverse ('"' : curr) : acc) "" False cs  -- End string
    tokenize' acc curr True (c:cs) = 
        tokenize' acc (c : curr) True cs  -- Continue in string
    
    tokenize' acc curr False ('"':cs) = 
        tokenize' acc ('"' : curr) True cs  -- Start string
    tokenize' acc curr False (c:cs)
        | isSpace c = if null curr 
                     then tokenize' acc curr False cs  -- Skip whitespace
                     else tokenize' (reverse curr : acc) "" False cs  -- End token
        | c `elem` "{}[]" = 
            if null curr
            then tokenize' ([c] : acc) "" False cs  -- Single char token
            else tokenize' ([c] : reverse curr : acc) "" False cs  -- End token, add special
        | otherwise = tokenize' acc (c : curr) False cs  -- Add to token