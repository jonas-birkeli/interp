module Main (main) where

import Lib
import System.Environment (getArgs)
import System.Directory (doesFileExist)

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
        else InvalidFileMode filepath

-- | Execution modes for the interpeter
data RunMode = ReplMode
    | FileMode FilePath
    | InvalidFileMode FilePath

-- | Execute a particular run mode
executeMode :: RunMode -> IO ()
executemode ReplMode = startRepl
executeMode (FileMode path) = runWithPrelude path
executeMode (InvalidFileMode path) = handleInvalidFile path
