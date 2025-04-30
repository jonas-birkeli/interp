module Interpreter.Core
    (
        executeProgram,
        executeTokenStream,
        executeToken,
        executeValue,
        executeQuotationOrValue,
        executeOperator,
        extractFinalValue,
        evaluateValue,
        evaluateList,
        splitAtQuotation
    ) where

import Types (ProgramError(..), State(..), Token(..), Value(..))
import qualified Data.Map as Map
import Interpreter.Stack (pushValue, popValue)
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

-- | Execute a stream of tokens
executeTokenStream :: [Token] -> State -> Either ProgramError (State, [Token])
executeTokenStream [] state = Right (state, [])
executeTokenStream (token:tokens) state = 
    case token of
        -- Control flow operations that need to handle the token stream
        IfToken -> executeIf tokens state
        TimesToken -> executeTimes tokens state
        LoopToken -> executeLoop tokens state
        -- Defered functions, needs token stream
        MapToken -> executeMap tokens state
        EachToken -> executeEach tokens state
        -- For regular tokens, process them and continue with the stream
        _ -> do
            state' <- executeToken token state
            executeTokenStream tokens state'

-- | Execute a single token with a given state
executeToken :: Token -> State -> Either ProgramError State
executeToken token s = case token of
    ValueToken value -> executeValue value s
    OperatorToken op -> executeOperator op s
    AssignmentToken -> executeAssignment s
    FunctionToken -> executeFunction s
    FoldlToken -> executeFoldl s
    ExecToken -> executeExec s
    -- Control flow tokens should be handled by executeTokenStream
    IfToken -> Left $ UnknownSymbol "if token encountered out of context"
    TimesToken -> Left $ UnknownSymbol "times token encountered out of context"
    LoopToken -> Left $ UnknownSymbol "loop token encountered out of context"

-- | Execute a value, handling symbol lookup
executeValue :: Value -> State -> Either ProgramError State
executeValue value s = case value of
    SymbolValue name ->
        -- Check for occurancei in dict
        case Map.lookup name (dictionary s) of
            Just quotation@(QuotationValue tokens) ->
                executeQuotationOrValue quotation s
            Just value' ->
            -- Push the value if its not an quotation
                Right $ pushValue value' s
            Nothing ->
                -- Push the symbol itself if not found
                Right $ pushValue value s
    _ -> Right $ pushValue value s

-- | Execute a quotation or a direct value
executeQuotationOrValue :: Value -> State -> Either ProgramError State
executeQuotationOrValue value state = case value of
    QuotationValue tokens -> 
        -- Execute the quotation as a series of tokens
        executeTokenStream tokens state >>= \(finalState, _) -> Right finalState
    -- For non-quotation values, just push onto the stack
    _ -> Right $ pushValue value state

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

-- | Split token stream at the next quotation or single value
splitAtQuotation :: [Token] -> Either ProgramError ([Token], [Token])
splitAtQuotation [] = Left $ UnknownSymbol "Expected quotation, found end of program"
splitAtQuotation (ValueToken (QuotationValue tokens):rest) = Right (tokens, rest)
splitAtQuotation (token:rest) = Right ([token], rest)