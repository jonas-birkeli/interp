module Lib
    ( 
        evalProgram,
        runFile,
        runREPL
    ) where

import Types
import Parser
import Interpreter
import System.IO
import Data.List (intercalate)

-- | Evalutae a single line in the context for current state
evalLine :: String -> State -> IO State
evalLine line state = case parseProgram line of
    Left err -> do
        putStrLn $ "Parse error: " ++ show err
        return state
    Right tokens -> do
        case executeTokenStream tokens state of
            Left err -> do
                putStrLn $ "Execution error: " ++ show err
                return state
            Right (state', _) -> do
                displayPrintBuffer state'

-- | Formats stack for display
formatStack :: Stack -> String
formatStack = ("[" ++ ) . (++ "]") . intercalate ", " . map show -- dotfree stack

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
    | input == ":clear" = do
        putStrLn "Stack cleared."
        pure $ Just state { stack = [] }
    | input == ":stack" = do
        putStrLn $ "Stack: " ++ formatStack ( stack state )
        pure $ Just state
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
    putStr "interp> "
    hFlush stdout

-- | Display the print buffer
displayPrintBuffer :: State -> IO State
displayPrintBuffer state = do
    -- Print each item in the buffer
    mapM_ putStrLn (printBuffer state)
    -- Return the state with an empty buffer
    return state { printBuffer = [] }

-- | Run a program from a file
runFile :: FilePath -> IO State
runFile filePath = do
    program <- readFile filePath
    putStrLn $ "Executing program from file: " ++ filePath
    evalProgram program initialState