module Interpreter.IO
    (
        executePrint,
        executeRead,
    ) where

import Types 
import Interpreter.Stack 

-- | Execute print operation
executePrint :: State -> Either ProgramError State
executePrint state = do
    (v, state') <- popValue state
    let buffer = printBuffer state' ++ [show v]
    return state' { printBuffer = buffer}

-- | Execute read operation
executeRead :: State -> Either ProgramError State
executeRead state = do
    -- Get from stdin? TODO
    return $ pushValue (StringValue "") state