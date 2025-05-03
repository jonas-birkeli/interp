module Parser.Tokenizer 
    (
        tokenize
    ) where

import Data.Char 

-- | Tokenize a string, respecting quoted strings, braces, and brackets, and handle comments
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
tokenize = reverse . filter (not . null) . fst . foldl processChar ([], (False, ""))
  where
    -- | Process each character in the input string
    -- Parameters:
    --   * (tokens, (inString, curr)): Current state with:
    --     - tokens: Accumulated tokens so far
    --     - inString: Whether we're inside a string
    --     - curr: Current token being built
    --   * char: Current character being processed
    processChar (tokens, (inString, curr)) char = case (inString, char) of
      -- Inside a string, encountered a closing quote
      (True, '"')  -> (reverse curr : tokens, (False, ""))
      
      -- Inside a string, encountered a backslash (escape character)
      (True, '\\') -> (tokens, (True, '\\' : curr))
      
      -- Inside a string, regular character
      (True, c)    -> (tokens, (True, c : curr))
      
      -- Not in a string, encountered an opening quote
      (False, '"') -> (tokens, (True, '"' : curr))
      
      -- Not in a string, other characters
      (False, c) 
        | isSpace c -> if null curr 
                      then (tokens, (False, ""))           -- Skip whitespace
                      else (reverse curr : tokens, (False, "")) -- End token
                      
        -- Special handling for brackets and braces
        | c `elem` "{}[]" -> 
            if null curr
            then ([c] : tokens, (False, ""))               -- Single char token
            else ([c] : reverse curr : tokens, (False, "")) -- End token + special char
            
        -- Regular character, add to current token
        | otherwise -> (tokens, (False, c : curr))