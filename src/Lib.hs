module Lib
    ( someFunc
    ) where

-- | Placeholder function
someFunc :: IO ()
someFunc = putStrLn "Bprog Interpreter"

-- | Evalutae a single line in the context for current state
evalLine :: String -> State -> IO State

-- | Show execution result with stack info
showExecutionResult :: (State, Value) -> IO ()
showExecutionResult (newstate, result) = do
    putStrLn $ "Result: " ++ show result 
    putSTrLn $ "Stack: " ++ formatStack (stack newState)

-- | Formats stack for display
formatStack :: Stack -> String
formatStack = ("[" ++ ) . (++ "]") . intercalate ", " . map show -- dotfree stack
-- (++ "] <- function , ("[" ++) <- function

-- | Handle error in parsing
handleParseError 

-- | Parse and execute program
parseAndExecute :: (String -> Either ParseError [Token])
                -> String
                -> State
                -> ((State, Value) -> IO ())
                -> IO State
parseAndExecute parser program state resultHandler =