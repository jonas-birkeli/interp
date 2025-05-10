module Main where

import Interp.Repl
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runRepl
        [file] -> runFile file
        _ -> putStrLn "Usage: interp [file]"