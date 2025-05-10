module Interp.Eval.Logic(
   execAnd,
   execOr,
   execNot
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Eval.Stack

-- | Logical AND operation
--
-- >>> let state = push (BoolValue True) $ push (BoolValue True) initialState
-- >>> fmap stack (execAnd state)
-- Right [BoolValue True]
--
-- >>> let state' = push (BoolValue True) $ push (BoolValue False) initialState
-- >>> fmap stack (execAnd state')
-- Right [BoolValue False]
--
execAnd :: InterpreterState -> Either InterpError InterpreterState
execAnd state = popN 2 state >>= \(values, state') ->
    case values of
        [v2, v1] ->
            case (v1, v2) of
                (BoolValue b1, BoolValue b2) -> Right $ push (BoolValue (b1 && b2)) state'
                _ -> Left . RuntimeError $ typeMismatch "boolean" v1
        _ -> Left $ RuntimeError StackUnderflow

-- | Logical OR operation
--
-- >>> let state = push (BoolValue False) $ push (BoolValue True) initialState
-- >>> fmap stack (execOr state)
-- Right [BoolValue True]
--
-- >>> let state' = push (BoolValue False) $ push (BoolValue False) initialState
-- >>> fmap stack (execOr state')
-- Right [BoolValue False]
--
execOr :: InterpreterState -> Either InterpError InterpreterState
execOr state = popN 2 state >>= \(values, state') ->
    case values of
        [v2, v1] -> 
            case (v1, v2) of
                (BoolValue b1, BoolValue b2) -> Right $ push (BoolValue (b1 || b2)) state'
                _ -> Left . RuntimeError $ typeMismatch "boolean" v1
        _ -> Left $ RuntimeError StackUnderflow

-- | Logical NOT operation
--
-- >>> let state = push (BoolValue True) initialState
-- >>> fmap stack (execNot state)
-- Right [BoolValue False]
--
-- >>> let state' = push (IntValue 10) initialState
-- >>> fmap stack (execNot state')
-- Right [IntValue (-10)]
--
execNot :: InterpreterState -> Either InterpError InterpreterState
execNot state = pop state >>= \(v, state') ->
    case v of
        BoolValue b -> Right $ push (BoolValue (not b)) state'
        IntValue i -> Right $ push (IntValue (-i)) state'
        _ -> Left . RuntimeError $ typeMismatch "boolean or integer" v