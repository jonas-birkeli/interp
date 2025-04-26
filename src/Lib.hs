module Lib
    ( 
        evalProgram,
        runFile,
        runREPL
    ) where


import Types
    ( ParseError, ProgramError(..), State(stack), Stack, Token, Value )
import Parser (parseProgram)
import Interpreter (initialState, executeProgram)
import System.IO (hFlush, stdout)
import Data.List (intercalate)

-- | Placeholder function
someFunc :: IO ()
someFunc = putStrLn "Stack Interpreter"

-- | Evalutae a single line in the context for current state
evalLine :: String -> State -> IO State
evalLine line state = case parseProgram line of
  Left err -> do
    putStrLn $ "Parse error: " ++ show err
    return state  -- Return the original state
  Right tokens -> case executeProgram tokens state of
    Left err -> do
      putStrLn $ "Execution error: " ++ show err
      return state { stack = [] }  -- Clear stack on error
    Right (newState, result) -> do
      putStrLn $ "Result: " ++ show result
      putStrLn $ "Stack: " ++ formatStack [result]  -- Show only the result
      return newState { stack = [] }  -- Clear stack after command

-- | Show execution result with stack info
showExecutionResult :: (State, Value) -> IO ()
showExecutionResult (newState, result) = do
    putStrLn $ "Result: " ++ show result 
    putStrLn $ "Stack: " ++ formatStack (stack newState)

-- | Formats stack for display
formatStack :: Stack -> String
formatStack = ("[" ++ ) . (++ "]") . intercalate ", " . map show -- dotfree stack
-- (++ "] <- function , ("[" ++) <- function

-- | Handle parse errors
handleParseError :: ParseError -> State -> IO State
handleParseError err state = do
    putStrLn $ "Parse error: " ++ show err
    pure state

-- | Handle execution errors
handleExecutionError :: ProgramError -> State -> IO State
handleExecutionError err state = do
    putStrLn $ "Execution error: " ++ show err
    pure state

-- | Parse and execute program
parseAndExecute :: (String -> Either ParseError [Token])
                -> String
                -> State
                -> ((State, Value) -> IO ())
                -> IO State
parseAndExecute parser program state resultHandler = 
    either (`handleParseError` state) executeTokens (parser program)
    where
        executeTokens tokens =
            either (`handleExecutionError` state)
            (\result -> resultHandler result >> pure (fst result))
            (executeProgram tokens state)

-- | Evaluate a full program
evalProgram :: String -> State -> IO State
evalProgram program state = parseAndExecute parseProgram program state displayFinalResult

-- | Display just the final result
displayFinalResult :: (State, Value) -> IO ()
displayFinalResult (_, result) = print result 

-- | Process a single REPL input
processReplInput :: String -> State -> IO (Maybe State)
processReplInput input state 
    | input `elem` [":q", ":quit"] = do
        putStrLn "Bye!"
        pure Nothing
    | otherwise = Just <$> evalLine input state

-- | Run the REPL (Read-Eval-Print Loop)
runREPL :: State -> IO ()
runREPL state = do
    promptForInput
    input <- getLine
    maybeState <- processReplInput input state
    maybe (pure ()) runREPL maybeState

-- | Display the REPL prompt
promptForInput :: IO ()
promptForInput = do
    putStrLn "stack> "
    hFlush stdout

-- | Run a program from a file
runFile :: FilePath -> IO State
runFile filePath = do
    program <- readFile filePath
    putStrLn $ "Executing program from file: " ++ filePath
    evalProgram program initialState