module Interp.Eval.Control(
   execIf,
   execTimes,
   execLoop,
   execExec
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Core.State
import Interp.Eval.Stack

-- | Execute if statement by modifying the program
--
-- >>> let state = initialState { program = [LiteralToken (QuotationValue [LiteralToken (IntValue 1)]), LiteralToken (QuotationValue [LiteralToken (IntValue 2)])] }
-- >>> let state' = push (BoolValue True) state
-- >>> fmap program (execIf state')
-- Right [LiteralToken (IntValue 1)]
--
-- >>> let state'' = push (BoolValue False) state
-- >>> fmap program (execIf state'')
-- Right [LiteralToken (IntValue 2)]
--
execIf :: InterpreterState -> Either InterpError InterpreterState
execIf state = case program state of
    (thenPart : elsePart : rest) ->
        pop state >>= \(cond, state') -> case cond of
            BoolValue True -> Right state' { program = expandBlock thenPart ++ rest }
            BoolValue False -> Right state' { program = expandBlock elsePart ++ rest }
            _ -> Left . RuntimeError $ typeMismatch "Bool" cond
    _ -> Left . RuntimeError $ typeMismatch "if with two blocks" (QuotationValue [])

-- | Execute times operation by modifying the program
--
-- >>> let state = initialState { program = [LiteralToken (QuotationValue [LiteralToken (IntValue 1)])] }
-- >>> let state' = push (IntValue 3) state
-- >>> fmap program (execTimes state')
-- Right [LiteralToken (IntValue 1),LiteralToken (IntValue 1),LiteralToken (IntValue 1)]
--
execTimes :: InterpreterState -> Either InterpError InterpreterState
execTimes state = case program state of
    (block : rest) ->
        pop state >>= \(count, state') -> case count of
            IntValue n | n >= 0 -> 
                Right state' { program = concat (replicate (fromInteger n) (expandBlock block)) ++ rest }
            _ -> Left . RuntimeError $ typeMismatch "non-negative integer" count
    _ -> Left . RuntimeError $ typeMismatch "times with block" (QuotationValue [])

-- | Execute loop operation by modifying the program
--
-- >>> let breakBlock = [LiteralToken (BoolValue True)]
-- >>> let bodyBlock = [LiteralToken (IntValue 1)]
-- >>> let state = initialState { program = [LiteralToken (QuotationValue breakBlock), LiteralToken (QuotationValue bodyBlock)] }
-- >>> fmap program (execLoop state)
-- Right [LiteralToken (BoolValue True),ControlToken If,LiteralToken (QuotationValue []),LiteralToken (QuotationValue [LiteralToken (IntValue 1),ControlToken Loop,LiteralToken (QuotationValue [LiteralToken (BoolValue True)]),LiteralToken (QuotationValue [LiteralToken (IntValue 1)])])]
--
execLoop :: InterpreterState -> Either InterpError InterpreterState
execLoop state = case program state of
    (breakCond : body : rest) ->
        let breakTokens = expandBlock breakCond
            bodyTokens = expandBlock body
            continueBlock = bodyTokens ++ [ControlToken Loop, breakCond, body]
            newProgram = breakTokens ++ 
                        [ControlToken If, 
                         LiteralToken (QuotationValue rest),
                         LiteralToken (QuotationValue (continueBlock ++ rest))]
        in Right state { program = newProgram }
    _ -> Left . RuntimeError $ typeMismatch "loop with break and body" (QuotationValue [])

-- | Execute exec operation
--
-- >>> let state = push (QuotationValue [LiteralToken (IntValue 42)]) initialState
-- >>> fmap program (execExec state)
-- Right [LiteralToken (IntValue 42)]
--
execExec :: InterpreterState -> Either InterpError InterpreterState
execExec state = pop state >>= \(v, state') -> case v of
    QuotationValue tokens -> Right state' { program = tokens ++ program state' }
    _ -> Left . RuntimeError $ typeMismatch "quotation" v

-- | Expand a block (quotation or single value)
expandBlock :: Token -> [Token]
expandBlock (LiteralToken (QuotationValue tokens)) = tokens
expandBlock token = [token]