module Main (main) where

import Lib
import Interpreter
import System.Environment
import System.Directory 
import Control.Monad.Extra
import Data.ByteString (fromFilePath)

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
executeMode (FileMode path) = do
    program <- loadProgram path
    runInterpreter initialState (programStep program)
executeMode (InvalidFileMode path) = handleInvalidFile path

-- | Load program with prelude
loadProgram :: FilePath -> IO String
loadProgram filePath = do
    prelude <- readFileSafe "stdlib/prelude.in"
    program <- readFileSafe filePath
    return $ prelude ++ "\n" ++ program

-- | Safely read a file, returning empty string if file doesn't exist
readFileSafe :: FilePath -> IO String
readFileSafe path = do
    fileExists <- doesFileExist path
    if fileExists
        then readFile path
        else return ""

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

-- | Run a file with prelude if available (Applicative style)
runWithPrelude :: FilePath -> IO ()
runWithPrelude filePath = do
    -- Use applicative to read both files
    program <- (\prelude user -> prelude ++ "\n" ++ user)
               <$> readFileSafe "stdlib/prelude.in"
               <*> readFileSafe filePath
    
    -- Run the combined program
    void $ evalProgram program initialState

