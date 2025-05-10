module Interp.Engine(
   parse,
   step,
   executeToken,
   executeOperator,
   executeControl,
   executeDefinition,
   runUntilComplete,
   extractFinalValue
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Core.State
import Interp.Parse.Tokenize
import Interp.Parse.Token
import Interp.Eval.Stack
import Interp.Eval.Arithmetic
import Interp.Eval.Comparison
import Interp.Eval.Logic
import Interp.Eval.Control
import Interp.Eval.Higher
import Interp.Eval.List
import Interp.Eval.String
import Interp.Eval.Variable
import Interp.Eval.IO
import qualified Data.Map as Map

-- | Parse a program from string
--
-- >>> parse "1 2 +"
-- Right [LiteralToken (IntValue 1),LiteralToken (IntValue 2),OperatorToken "+"]
--
-- >>> parse "True if { 1 } { 2 }"
-- Right [LiteralToken (BoolValue True),ControlToken If,LiteralToken (QuotationValue [LiteralToken (IntValue 1)]),LiteralToken (QuotationValue [LiteralToken (IntValue 2)])]
--
parse :: String -> Either InterpError Program
parse input = splitPreserveTokens input >>= parseProgram

-- | Execute a single step of the interpreter
--
-- >>> let state = initialState { program = [LiteralToken (IntValue 42)] }
-- >>> fmap stack (step state)
-- Right [IntValue 42]
--
-- >>> let state' = initialState { program = [OperatorToken "+"] }
-- >>> step state'
-- Left (RuntimeError StackUnderflow)
--
step :: InterpreterState -> Either InterpError InterpreterState
step state = case program state of
    [] -> Right state
    (tok:rest) -> executeToken tok state { program = rest }

-- | Execute a token
executeToken :: Token -> InterpreterState -> Either InterpError InterpreterState
executeToken (LiteralToken (SymbolValue name)) state = execSymbol name state
executeToken (LiteralToken (ListValue values)) state = 
    -- Evaluate list values by resolving symbols
    let evaluatedValues = map (evaluateValue state) values
    in Right $ push (ListValue evaluatedValues) state
executeToken (LiteralToken value) state = Right $ push value state
executeToken (OperatorToken op) state = executeOperator op state
executeToken (ControlToken ctrl) state = executeControl ctrl state
executeToken (DefinitionToken def) state = executeDefinition def state

-- | Evaluate a value, resolving symbols
evaluateValue :: InterpreterState -> Value -> Value
evaluateValue state (SymbolValue name) = case Map.lookup name (dict state) of
    Just value -> value
    Nothing -> SymbolValue name
evaluateValue state (ListValue values) = ListValue (map (evaluateValue state) values)
evaluateValue _ value = value

-- | Execute an operator
executeOperator :: String -> InterpreterState -> Either InterpError InterpreterState
executeOperator op = case op of
    -- Arithmetic
    "+" -> execAdd
    "-" -> execSub
    "*" -> execMul
    "/" -> execFloatDiv
    "div" -> execIntDiv
    
    -- Comparison
    "<" -> execLt
    ">" -> execGt
    "==" -> execEq
    
    -- Logic
    "&&" -> execAnd
    "||" -> execOr
    "not" -> execNot
    
    -- Stack
    "dup" -> execDup
    "swap" -> execSwap
    "pop" -> execPop
    
    -- I/O
    "print" -> execPrint
    "read" -> execRead
    
    -- String
    "parseInteger" -> execParseInteger
    "parseFloat" -> execParseFloat
    "words" -> execWords
    
    -- List
    "head" -> execHead
    "tail" -> execTail
    "empty" -> execEmpty
    "length" -> execLength
    "cons" -> execCons
    "append" -> execAppend
    
    _ -> \_ -> Left . ParseError $ UnknownToken op

-- | Execute a control operation
executeControl :: ControlOp -> InterpreterState -> Either InterpError InterpreterState
executeControl ctrl = case ctrl of
    If -> execIf
    Times -> execTimes
    Loop -> execLoop
    Each -> execEach
    Map -> execMap
    Foldl -> execFoldl
    Exec -> execExec

-- | Execute a definition operation
executeDefinition :: DefOp -> InterpreterState -> Either InterpError InterpreterState
executeDefinition def = case def of
    Assign -> execAssign
    FunDef -> execFunDef

-- | Run a program until completion or interrupt
--
-- >>> runUntilComplete initialState { program = [LiteralToken (IntValue 1), LiteralToken (IntValue 2), OperatorToken "+"] }
-- Right (InterpreterState {dict = fromList [], stack = [IntValue 3], program = [], interrupt = Nothing})
--
runUntilComplete :: InterpreterState -> Either InterpError InterpreterState
runUntilComplete state
    | null (program state) = extractFinalValue state
    | Just _ <- interrupt state = Right state
    | otherwise = step state >>= runUntilComplete

-- | Extract the final value from the stack
--
-- >>> extractFinalValue initialState { stack = [IntValue 42] }
-- Right (InterpreterState {dict = fromList [], stack = [IntValue 42], program = [], interrupt = Nothing})
--
-- >>> extractFinalValue initialState { stack = [] }
-- Left (RuntimeError ProgramFinishedWithNoValues)
--
-- >>> extractFinalValue initialState { stack = [IntValue 1, IntValue 2] }
-- Left (RuntimeError (ProgramFinishedWithMultipleValues [IntValue 1,IntValue 2]))
--
extractFinalValue :: InterpreterState -> Either InterpError InterpreterState
extractFinalValue state = case stack state of
    [] -> Left $ RuntimeError ProgramFinishedWithNoValues
    [_] -> Right state
    values -> Left $ RuntimeError $ ProgramFinishedWithMultipleValues values