module Interpreter.IO
    (
        executePrint,
        executeRead,
    ) where

import Types (ProgramError(..), State(..), Value(..))
import Interpreter.Stack (pushValue, popValue)

-- | Execute print operation
executePrint :: State -> Either ProgramError State
executePrint state = do
    (value, state') <- popValue state
    -- Print to stdout? TODO
    return state'

-- | Execute read operation
executeRead :: State -> Either ProgramError State
executeRead state = do
    -- Get from stdin? TODO
    return $ pushValue (StringValue "") state