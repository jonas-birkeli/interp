module Main (main) where

import Lib
import Interpreter
import System.Environment
import System.Directory
import Control.Monad.Extra
import Text.ParserCombinators.ReadPrec ()
import Types
import Parser
import GHC.IO.FD ()
import System.IO


-- | The main entry point
main :: IO ()
main = getArgs >>= determineRunMode >>= executeMode

-- | Determine the run mode based on command line arguments - File or not basically
determineRunMode :: [String] -> IO Types.RunMode
determineRunMode [] = pure Types.ReplMode
determineRunMode (filePath:_) = do
    fileExists <- doesFileExist filePath
    pure $ if fileExists
        then Types.FileMode filePath
        else Types.InvalidFileMode filePath

-- | Execute a particular run mode
executeMode :: RunMode -> IO ()
executeMode ReplMode = runInterpreter initialState (replStep "") -- No program attached to execution
executeMode (FileMode path) = do
    program <- loadProgram path
    runInterpreter initialState (programStep program)
executeMode (InvalidFileMode path) = putStrLn $ "Did not find file: "++ path

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

-- | Run interpreter with given stepping function
runInterpreter :: State -> (State -> IO (Maybe (State, [Token]))) -> IO ()
runInterpreter state step = do
    maybeNewState <- step state
    case maybeNewState of
        Just (newState, remainingTokens) ->
            if null remainingTokens
                then return ()
                else runInterpreter newState (processToken remainingTokens)

             --runInterpreter newState step
        Nothing -> return ()

-- | Repl step function
replStep :: String -> State -> IO (Maybe (State, [Token]))
replStep _ state = do
    print $ show state

    putStr "interp> "
    hFlush stdout
    input <- getLine

    case input of
        ":q"    -> return Nothing -- quit
        ":quit" -> return Nothing -- quit
        ":clear" -> return $ Just (state { stack = [] }, []) -- clear stack
        ":stack" -> return Nothing -- TODO
        _ -> interpretLine input state

-- | Program step function
programStep :: String -> State -> IO (Maybe (State, [Token]))
programStep program state = do
    -- Parse program
    case parseProgram program of
        Left _ -> do
            return Nothing
        Right tokens -> processToken tokens state

-- | Process tokens one by one
processToken :: [Token] -> State -> IO (Maybe (State, [Token]))
processToken [] state = do
    -- Execute no token, eg. end of process
    case extractFinalValue state of
        Left err -> do
            putStrLn $ "Error: " ++ show err
            return Nothing
        Right (_, value) -> do
            putStrLn $ "Return value: " ++ show value
            return Nothing
processToken tokens state = do
    -- Execute single token, handle input and output
    case executeToken tokens state of
        Left err -> do
            putStrLn $ "Error: " ++ show err
            return Nothing
        Right (state', remainingTokens) -> do
            handlePrintBuffer state' >>= handleRead >>= processToken remainingTokens

-- | Interpret a single line
interpretLine :: String -> State -> IO (Maybe (State, [Token]))
interpretLine line state = do
    case parseProgram line of
        Left err -> do
            -- Handle parse error
            return $ Just (state, [])
        Right tokens -> do
            case executeToken tokens state of
                Left err -> do
                    -- Handle execution error
                    return $ Just (state, tokens)
                Right (state', remainingTokens) -> do
                    handlePrintBuffer state' >>= handleRead >>= processToken remainingTokens

-- | Handle print buffer display
handlePrintBuffer :: State -> IO State
handlePrintBuffer state = do
    mapM_ putStrLn (printBuffer state)
    return state { printBuffer = [] }


-- | Handle reading to stack
handleRead :: State -> IO State
handleRead state =
    if requestRead state then do
        putStrLn "input> "
        hFlush stdout
        input <- getLine

        return $ pushValue (StringValue input) state { requestRead = False }
    else return state