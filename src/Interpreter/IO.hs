module Interpreter.IO
    (
        executePrint,
        executeRead,
        executeExec
    ) where

import Types (ProgramError(..), State(..), Value(..))
import Interpreter.Stack (pushValue, popValue)
import Interpreter.Core (executeTokenStream)

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

-- | Execute exec operation
executeExec :: State -> Either ProgramError State
executeExec state = do
    (quotation, state') <- popValue state
    case quotation of
        QuotationValue tokens -> do
            (finalState, _) <- executeTokenStream tokens state'
            return finalState
        _ -> Left $ ExpectedQuotation quotation