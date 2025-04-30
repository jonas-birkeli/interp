module Interpreter.Variables
    (
        executeAssignment,
        executeFunction
    ) where

import Types (ProgramError(..), State(..), Value(..))
import Interpreter.Stack (popValue)
import qualified Data.Map as Map

-- | Execute asisgnment operation (:=)
executeAssignment :: State -> Either ProgramError State
executeAssignment state = do
    (value, state1) <- popValue state
    (symbolValue, state2) <- popValue state1
    case symbolValue of
        SymbolValue name ->
            Right $ state2 { dictionary = Map.insert name value (dictionary state2) }
        _ -> Left $ ExpectedVariable symbolValue

-- | Execute function definition operator (fun)
executeFunction :: State -> Either ProgramError State
executeFunction state = do
    (quotation, state1) <- popValue state
    (symbolValue, state2) <- popValue state1
    case (quotation, symbolValue) of
        (QuotationValue _, SymbolValue name) ->
            Right $ state2 { dictionary = Map.insert name quotation (dictionary state2 )}
        (_, _) -> Left $ ExpectedQuotation quotation