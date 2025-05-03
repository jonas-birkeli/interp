module Main (main) where

import Lib
import Interpreter
import System.Environment
import System.Directory 
import Control.Monad.Extra

-- | The main entry point
main :: IO ()
main = getArgs >>= determineRunMode >>= executeMode

-- | Determine the run mode based on command line arguments - File or not basically
determineRunMode :: [String] -> IO RunMode
determineRunMode [] = pure ReplMode
determineRunMode (filePath:_) = do
    fileExists <- doesFileExist filePath
    pure $ if fileExists
        then FileMode filePath
        else InvalidFileMode filePath

-- | Execution modes for the interpeter
data RunMode = ReplMode
    | FileMode FilePath
    | InvalidFileMode FilePath

-- | Execute a particular run mode
executeMode :: RunMode -> IO ()
executeMode ReplMode = startRepl
executeMode (FileMode path) = runWithPrelude path
executeMode (InvalidFileMode path) = handleInvalidFile path

-- | Start the REPL
startRepl :: IO ()
startRepl = do
    mapM_ putStrLn welcomeMessages
    runREPL initialState

-- | Welcome messages for REPL mode
welcomeMessages :: [String]
welcomeMessages = 
    [ 
        "",
        "You have now entered REPL mode",
        "Commands: ",
        "':stack' to show stack",
        "':clear' to clear stack",
        "':q' or ':quit' to exit.",
        ""
    ]

-- | Handle an invalid file path
handleInvalidFile :: FilePath -> IO ()
handleInvalidFile path = do
    mapM_ putStrLn $ invalidFileMessages path
    startRepl

-- | Invalid file message for when provided invalid file
invalidFileMessages :: FilePath -> [String]
invalidFileMessages path =
    [
        "File not found: " ++ path,
        "Starting REPL mode instead"
    ]

-- | Run a file with prelude if available
runWithPrelude :: FilePath -> IO ()
runWithPrelude filePath = do
    -- Check if prelude exists
    preludeExists <- doesFileExist preludePath
    -- Read user program
    userProgram <- readFile filePath
    program <- if preludeExists
        then do
            preludeCode <- readFile preludePath
            -- Combine prelude with program
            return $ preludeCode ++ "\n" ++ userProgram
        else
            return userProgram
    
    -- Run program and ignore result
    void $ evalProgram program initialState
  where
    preludePath = "stdlib/prelude.in"