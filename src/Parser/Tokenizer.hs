module Tokenizer 
    (
        tokenize
    ) where

import Data.Char (isSpace)

-- | Tokenize a string, respecting quoted strings, braces, and brackets
--
-- >>> tokenize "1 2 +"
-- ["1","2","+"]
--
-- >>> tokenize "1 2 + 3 *"
-- ["1","2","+","3","*"]
--
-- >>> tokenize "\" hello world \""
-- ["\" hello world \""]
--
-- >>> tokenize "\" string with \\\" escaped quotes \""
-- ["\" string with \\\" escaped quotes \""]
--
-- >>> tokenize "10 20 == if { \" equal \" println } { \" not equal \" println }"
-- ["10","20","==","if","{","\" equal \"","println","}","{","\" not equal \"","println","}"]
--
-- >>> tokenize "{ 1 2 + }"
-- ["{","1","2","+","}"]
--
-- >>> tokenize "[ 1 \" two \" 3 ]"
-- ["[","1","\" two \"","3","]"]
--
-- >>> tokenize "[ ] empty"
-- ["[","]","empty"]
--
-- >>> tokenize "name \" John Doe \" :="
-- ["name","\" John Doe \"",":="]
--
-- >>> tokenize "3 10 > if { \" 3 is more than 10 \" println } { \" 3 is less than 10 \" println }"
-- ["3","10",">","if","{","\" 3 is more than 10 \"","println","}","{","\" 3 is less than 10 \"","println","}"]
--
-- >>> tokenize "{{ nested } quotation }"
-- ["{","{","nested","}","quotation","}"]
--
-- >>> tokenize ""
-- []
--
-- >>> tokenize "    "
-- []
tokenize :: String -> [String]
tokenize input = tokenize' [] "" False False input
  where
    -- tokenize' accumulator current_token in_string escaped_char input
    tokenize' acc curr _ _ [] 
        | null curr = reverse acc
        | otherwise = reverse (reverse curr : acc)
    
    -- Handle escaped characters in string
    tokenize' acc curr True True (c:cs) = 
        tokenize' acc (c : curr) True False cs  -- Add escaped char and continue
    
    -- Handle escape character
    tokenize' acc curr True False ('\\':cs) = 
        tokenize' acc ('\\' : curr) True True cs  -- Mark next char as escaped
    
    -- End of string
    tokenize' acc curr True False ('"':cs) = 
        tokenize' (reverse ('"' : curr) : acc) "" False False cs  -- End string
    
    -- Continue in string
    tokenize' acc curr True False (c:cs) = 
        tokenize' acc (c : curr) True False cs  -- Continue in string
    
    -- Start string
    tokenize' acc curr False False ('"':cs) = 
        tokenize' acc ('"' : curr) True False cs  -- Start string
    
    -- Handle normal state
    tokenize' acc curr False False (c:cs)
        | isSpace c = if null curr 
                     then tokenize' acc curr False False cs  -- Skip whitespace
                     else tokenize' (reverse curr : acc) "" False False cs  -- End token
        | c `elem` "{}[]" = 
            if null curr
            then tokenize' ([c] : acc) "" False False cs  -- Single char token
            else tokenize' ([c] : reverse curr : acc) "" False False cs  -- End token, add special
        | otherwise = tokenize' acc (c : curr) False False cs  -- Add to token