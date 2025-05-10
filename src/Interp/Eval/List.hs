module Interp.Eval.List(
   execHead,
   execTail,
   execEmpty,
   execLength,
   execCons,
   execAppend
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Eval.Stack

-- | Execute length operation
--
-- >>> let state = push (ListValue [IntValue 1, IntValue 2, IntValue 3]) initialState
-- >>> fmap stack (execLength state)
-- Right [IntValue 3]
--
-- >>> let state' = push (StringValue "hello") initialState
-- >>> fmap stack (execLength state')
-- Right [IntValue 5]
--
-- >>> let state'' = push (QuotationValue [LiteralToken (IntValue 1), OperatorToken "+"]) initialState
-- >>> fmap stack (execLength state'')
-- Right [IntValue 2]
--
execLength :: InterpreterState -> Either InterpError InterpreterState
execLength state = pop state >>= \(v, state') -> case v of
    ListValue xs -> Right $ push (IntValue . fromIntegral $ length xs) state'
    StringValue s -> Right $ push (IntValue . fromIntegral $ length s) state'
    QuotationValue ts -> Right $ push (IntValue . fromIntegral $ length ts) state'
    _ -> Left . RuntimeError $ typeMismatch "list, string, or quotation" v

-- | Execute empty operation
--
-- >>> let state = push (ListValue []) initialState
-- >>> fmap stack (execEmpty state)
-- Right [BoolValue True]
--
-- >>> let state' = push (ListValue [IntValue 1]) initialState
-- >>> fmap stack (execEmpty state')
-- Right [BoolValue False]
--
execEmpty :: InterpreterState -> Either InterpError InterpreterState
execEmpty state = pop state >>= \(v, state') -> case v of
    ListValue [] -> Right $ push (BoolValue True) state'
    ListValue _ -> Right $ push (BoolValue False) state'
    _ -> Left . RuntimeError $ typeMismatch "list" v

-- | Execute head operation
--
-- >>> let state = push (ListValue [IntValue 1, IntValue 2, IntValue 3]) initialState
-- >>> fmap stack (execHead state)
-- Right [IntValue 1]
--
-- >>> let state' = push (ListValue []) initialState
-- >>> execHead state'
-- Left (RuntimeError (TypeMismatch "non-empty list" (ListValue [])))
--
execHead :: InterpreterState -> Either InterpError InterpreterState
execHead state = pop state >>= \(v, state') -> case v of
    ListValue [] -> Left . RuntimeError $ typeMismatch "non-empty list" v
    ListValue (x:_) -> Right $ push x state'
    _ -> Left . RuntimeError $ typeMismatch "list" v

-- | Execute tail operation
--
-- >>> let state = push (ListValue [IntValue 1, IntValue 2, IntValue 3]) initialState
-- >>> fmap stack (execTail state)
-- Right [ListValue [IntValue 2,IntValue 3]]
--
-- >>> let state' = push (ListValue []) initialState
-- >>> execTail state'
-- Left (RuntimeError (TypeMismatch "non-empty list" (ListValue [])))
--
execTail :: InterpreterState -> Either InterpError InterpreterState
execTail state = pop state >>= \(v, state') -> case v of
    ListValue [] -> Left . RuntimeError $ typeMismatch "non-empty list" v
    ListValue (_:xs) -> Right $ push (ListValue xs) state'
    _ -> Left . RuntimeError $ typeMismatch "list" v

-- | Execute cons operation
--
-- >>> let state = push (ListValue [IntValue 2, IntValue 3]) $ push (IntValue 1) initialState
-- >>> fmap stack (execCons state)
-- Right [ListValue [IntValue 1,IntValue 2,IntValue 3]]
--
execCons :: InterpreterState -> Either InterpError InterpreterState
execCons state = pop state >>= \(xs, state1) ->
    pop state1 >>= \(x, state2) -> case xs of
        ListValue items -> Right $ push (ListValue (x : items)) state2
        _ -> Left . RuntimeError $ typeMismatch "list" xs

-- | Execute append operation
--
-- >>> let state = push (ListValue [IntValue 3, IntValue 4]) $ push (ListValue [IntValue 1, IntValue 2]) initialState
-- >>> fmap stack (execAppend state)
-- Right [ListValue [IntValue 1,IntValue 2,IntValue 3,IntValue 4]]
--
execAppend :: InterpreterState -> Either InterpError InterpreterState
execAppend state = pop state >>= \(ys, state1) ->
    pop state1 >>= \(xs, state2) -> case (xs, ys) of
        (ListValue xs', ListValue ys') -> Right $ push (ListValue (xs' ++ ys')) state2
        _ -> Left . RuntimeError $ typeMismatch "list" xs