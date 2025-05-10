module Interp.Repl(
   runRepl,
   replLoop,
   ReplResult(..),
   processInput,
   executeLine,
   runWithIO,
   printStack,
   runFile,
   loadPrelude
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Core.State
import Interp.Engine
import Interp.Eval.Variable
import System.IO
import System.Directory (doesFileExist)
import qualified Data.Map as Map

-- | Run the REPL
runRepl :: IO ()
runRepl = replLoop initialState

-- | Main REPL loop
replLoop :: InterpreterState -> IO ()
replLoop state = do
    putStr "interp> "
    hFlush stdout
    input <- getLine
    result <- processInput input state
    case result of
        Exit -> putStrLn "Bye!"
        Continue state' -> replLoop state'

-- | Result of processing input
data ReplResult = Exit | Continue InterpreterState

-- | Process REPL input
processInput :: String -> InterpreterState -> IO ReplResult
processInput input state
    | input `elem` [":q", ":quit"] = return Exit
    | input == ":clear" = do
        putStrLn "Stack cleared."
        return $ Continue state { stack = [] }
    | input == ":stack" = do
        printStack state
        return $ Continue state
    | otherwise = executeLine input state

-- | Execute a line of input
executeLine :: String -> InterpreterState -> IO ReplResult
executeLine input state = case parse input of
    Left err -> do
        putStrLn $ "Error: " ++ show err
        return $ Continue state
    Right prog -> do
        let state' = state { program = prog }
        result <- runWithIO state'
        case result of
            Left err -> do
                putStrLn $ "Error: " ++ show err
                return $ Continue state
            Right finalState -> do
                printStack finalState
                return $ Continue finalState

-- | Run interpreter with IO handling
runWithIO :: InterpreterState -> IO (Either InterpError InterpreterState)
runWithIO state = case runUntilComplete state of
    Left err -> return $ Left err
    Right state' -> case interrupt state' of
        Nothing -> return $ Right state'
        Just (PrintInterrupt msg) -> do
            putStrLn msg
            runWithIO state' { interrupt = Nothing }
        Just (ReadInterrupt cont) -> do
            putStr "input> "
            hFlush stdout
            input <- getLine
            runWithIO (cont input) { interrupt = Nothing }

-- | Print the current stack
printStack :: InterpreterState -> IO ()
printStack state = putStrLn $ "Stack: " ++ show (reverse $ stack state)

-- | Run a file
runFile :: FilePath -> IO ()
runFile path = do
    content <- readFile path
    case parse content of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right prog -> do
            let state = initialState { program = prog }
            result <- runWithIO state
            case result of
                Left err -> putStrLn $ "Runtime error: " ++ show err
                Right finalState -> case stack finalState of
                    [value] -> print value
                    [] -> putStrLn "No result"
                    values -> putStrLn $ "Multiple values: " ++ show values

-- | Load prelude
loadPrelude :: FilePath -> InterpreterState -> IO InterpreterState
loadPrelude path state = do
    exists <- doesFileExist path
    if exists then do
        content <- readFile path
        case parse content of
            Left _ -> return state
            Right prog -> case runUntilComplete state { program = prog } of
                Left _ -> return state
                Right state' -> return state' { stack = [], program = [] }
    else return state