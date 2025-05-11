{-# LANGUAGE RankNTypes #-}
module Interp.Eval.Comparison(
   binaryComparison,
   applyComparison,
   execEq,
   execLt,
   execGt
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Eval.Stack

-- | Binary comparison operation
binaryComparison :: (forall a. Ord a => a -> a -> Bool)
                -> InterpreterState
                -> Either InterpError InterpreterState
binaryComparison op state = popN 2 state >>= \(values, state') ->
    case values of
        [v2, v1] -> applyComparison op v1 v2 >>= \result -> Right (push (BoolValue result) state')
        _ -> Left $ RuntimeError StackUnderflow

-- | Apply comparison to values
--
-- >>> applyComparison (<) (IntValue 5) (IntValue 10)
-- Right True
--
-- >>> applyComparison (>) (FloatValue 3.14) (IntValue 3)
-- Right True
--
-- >>> applyComparison (<) (BoolValue True) (IntValue 1)
-- Left (RuntimeError (TypeMismatch "comparable numbers" (BoolValue True)))
--
applyComparison :: (forall a. Ord a => a -> a -> Bool) -> Value -> Value -> Either InterpError Bool
applyComparison op v1 v2 = case (v1, v2) of
    (IntValue i1, IntValue i2) -> Right $ op i1 i2
    (IntValue i1, FloatValue f2) -> Right $ op (fromIntegral i1) f2
    (FloatValue f1, IntValue i2) -> Right $ op f1 (fromIntegral i2)
    (FloatValue f1, FloatValue f2) -> Right $ op f1 f2
    _ -> Left . RuntimeError $ typeMismatch "comparable numbers" v1

-- | Equality comparison
--
-- >>> let state = push (IntValue 5) $ push (IntValue 5) initialState
-- >>> fmap stack (execEq state)
-- Right [BoolValue True]
--
-- >>> let state' = push (IntValue 5) $ push (FloatValue 5.0) initialState
-- >>> fmap stack (execEq state')
-- Right [BoolValue True]
--
-- >>> let state'' = push (StringValue "hello") $ push (StringValue "hello") initialState
-- >>> fmap stack (execEq state'')
-- Right [BoolValue True]
--
execEq :: InterpreterState -> Either InterpError InterpreterState
execEq state = popN 2 state >>= \(values, state') ->
    case values of
        [v2, v1] -> let result = case (v1, v2) of
                            (IntValue i1, FloatValue f2) -> fromIntegral i1 == f2
                            (FloatValue f1, IntValue i2) -> f1 == fromIntegral i2
                            _ -> v1 == v2
                    in Right (push (BoolValue result) state')
        _ -> Left $ RuntimeError StackUnderflow

-- | Comparison operations
execLt :: InterpreterState -> Either InterpError InterpreterState
execLt = binaryComparison (<)

execGt :: InterpreterState -> Either InterpError InterpreterState
execGt = binaryComparison (>)