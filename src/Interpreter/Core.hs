module Interpreter.Core
    (
        executeProgram,
        executeOperator,
        extractFinalValue,
        evaluateValue,
        evaluateList
    ) where

import Types
import qualified Data.Map as Map
import Interpreter.Stack
import Interpreter.Arithmetic
import Interpreter.Comparison
import Interpreter.Control
import Interpreter.HigherOrder
import Interpreter.Stack
import Interpreter.IO

-- | Execute the program (list of tokens) with given state
executeProgram :: [Token] -> State -> Either ProgramError (State, Value)
executeProgram tokens state = do
    (finalState, _) <- executeTokenStream tokens state
    (finalState', value) <- extractFinalValue finalState
    let evaluatedValue = evaluateValue value finalState'
    return (finalState, evaluatedValue)

-- | Execute operator token
executeOperator :: String -> State -> Either ProgramError State
executeOperator op = case op of
    "+" -> executeArithmetic (+)
    "-" -> executeArithmetic (-)
    "*" -> executeArithmetic (*)
    "div" -> executeIntegerDivision
    "/" -> executeFloatDivision
    "<" -> executeComparison (<)
    ">" -> executeComparison (>)
    "==" -> executeEquality
    "&&" -> executeLogical (&&)
    "||" -> executeLogical (||)
    "not" -> executeNot
    "dup" -> executeDup
    "swap" -> executeSwap
    "pop" -> executePop
    "parseInteger" -> executeParseInteger
    "parseFloat" -> executeParseFloat
    "words" -> executeWords
    "head" -> executeHead
    "tail" -> executeTail
    "empty" -> executeEmpty
    "length" -> executeLength
    "cons" -> executeCons
    "append" -> executeAppend
    "print" -> executePrint
    "read" -> executeRead
    _ -> \s -> Left $ UnknownSymbol op

-- | Extract the final value from state
extractFinalValue :: State -> Either ProgramError (State, Value)
extractFinalValue state = case stack state of
    [] -> Left ProgramFinishedWithNoValues
    [v] -> Right (state, v)
    vs -> Left (ProgramFinishedWithMultipleValues vs)

-- | Evaluate a value, resolving symbols in lists
evaluateValue :: Value -> State -> Value
evaluateValue (ListValue lv) state = ListValue (evaluateList lv state)
evaluateValue value _ = value

-- | Evaluate a list by replacing symbols with their values
evaluateList :: [Value] -> State -> [Value]
evaluateList [] _ = []
evaluateList (SymbolValue name : rest) state =
    case Map.lookup name (dictionary state) of
        Just value -> value : evaluateList rest state
        Nothing -> SymbolValue name : evaluateList rest state
evaluateList (value : rest) state = value : evaluateList rest state