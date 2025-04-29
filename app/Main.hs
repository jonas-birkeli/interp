module Main (main) where

import Types (State)
import Lib (evalProgram, runFile, runREPL)
import Interpreter (initialState)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.Maybe (fromMaybe)
import Control.Monad.Extra (ifM, void)

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
        "You have now entered REPL mode",
        "Enter ':q' or ':quit' to exit."
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
runWithPrelude filePath = void $ loadPrelude >>= maybe (runFile filePath) (loadAndRunFile filePath)

-- | Load the prelude file if it exists
loadPrelude :: IO (Maybe (IO State))
loadPrelude = ifM (doesFileExist "prelude.i") -- Monadic if-statementt :)
    (putStrLn "Loading prelude..." >> Just . (`evalProgram` initialState) <$> readFile "prelude.in") -- Wrap in Just . because maybe
    (pure Nothing)

-- | Load and run file with a state
loadAndRunFile :: FilePath -> IO State -> IO State
loadAndRunFile filePath getInitialState = 
    getInitialState >>= \state -> readFile filePath >>= flip evalProgram state