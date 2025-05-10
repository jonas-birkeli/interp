module Interp.Eval.Arithmetic(
   binaryArithmetic,
   applyArithmetic,
   execIntDiv,
   applyIntDiv,
   execFloatDiv,
   applyFloatDiv,
   execAdd,
   execSub,
   execMul
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Eval.Stack

-- | Binary arithmetic operation
binaryArithmetic :: (Double -> Double -> Double) 
                -> InterpreterState 
                -> Either InterpError InterpreterState
binaryArithmetic op state = popN 2 state >>= \([v2, v1], state') ->
    applyArithmetic op v1 v2 >>= \result -> Right (push result state')

-- | Apply arithmetic operation to values
--
-- >>> applyArithmetic (+) (IntValue 5) (IntValue 3)
-- Right (IntValue 8)
--
-- >>> applyArithmetic (*) (IntValue 5) (FloatValue 2.5)
-- Right (FloatValue 12.5)
--
-- >>> applyArithmetic (-) (FloatValue 10.0) (IntValue 3)
-- Right (FloatValue 7.0)
--
-- >>> applyArithmetic (+) (BoolValue True) (IntValue 1)
-- Left (RuntimeError (TypeMismatch "number" (BoolValue True)))
--
applyArithmetic :: (Double -> Double -> Double) -> Value -> Value -> Either InterpError Value
applyArithmetic op v1 v2 = case (v1, v2) of
    (IntValue i1, IntValue i2) -> 
        Right . IntValue . round $ op (fromIntegral i1) (fromIntegral i2)
    (IntValue i1, FloatValue f2) -> 
        Right . FloatValue $ op (fromIntegral i1) f2
    (FloatValue f1, IntValue i2) -> 
        Right . FloatValue $ op f1 (fromIntegral i2)
    (FloatValue f1, FloatValue f2) -> 
        Right . FloatValue $ op f1 f2
    _ -> Left . RuntimeError $ typeMismatch "number" v1

-- | Integer division
--
-- >>> let state = push (IntValue 10) $ push (IntValue 3) initialState
-- >>> fmap stack (execIntDiv state)
-- Right [IntValue 3]
--
-- >>> let state' = push (IntValue 1) $ push (IntValue 0) initialState
-- >>> execIntDiv state'
-- Left (RuntimeError DivisionByZero)
--
execIntDiv :: InterpreterState -> Either InterpError InterpreterState
execIntDiv state = popN 2 state >>= \([v2, v1], state') ->
    applyIntDiv v1 v2 >>= \result -> Right (push result state')

-- | Apply integer division
--
-- >>> applyIntDiv (IntValue 10) (IntValue 3)
-- Right (IntValue 3)
--
-- >>> applyIntDiv (FloatValue 10.5) (IntValue 3)
-- Right (IntValue 3)
--
-- >>> applyIntDiv (IntValue 10) (IntValue 0)
-- Left (RuntimeError DivisionByZero)
--
applyIntDiv :: Value -> Value -> Either InterpError Value
applyIntDiv v1 v2 = case (v1, v2) of
    (_, IntValue 0) -> Left $ RuntimeError DivisionByZero
    (_, FloatValue 0.0) -> Left $ RuntimeError DivisionByZero
    (IntValue i1, IntValue i2) -> Right $ IntValue (i1 `div` i2)
    (IntValue i1, FloatValue f2) -> Right $ IntValue (i1 `div` round f2)
    (FloatValue f1, IntValue i2) -> Right $ IntValue (round f1 `div` i2)
    (FloatValue f1, FloatValue f2) -> Right $ IntValue (round f1 `div` round f2)
    _ -> Left . RuntimeError $ typeMismatch "number" v1

-- | Float division
--
-- >>> let state = push (IntValue 10) $ push (IntValue 3) initialState
-- >>> fmap stack (execFloatDiv state)
-- Right [FloatValue 3.3333333333333335]
--
execFloatDiv :: InterpreterState -> Either InterpError InterpreterState
execFloatDiv state = popN 2 state >>= \([v2, v1], state') ->
    applyFloatDiv v1 v2 >>= \result -> Right (push result state')

-- | Apply float division
--
-- >>> applyFloatDiv (IntValue 10) (IntValue 3)
-- Right (FloatValue 3.3333333333333335)
--
-- >>> applyFloatDiv (IntValue 10) (FloatValue 0.0)
-- Left (RuntimeError DivisionByZero)
--
applyFloatDiv :: Value -> Value -> Either InterpError Value
applyFloatDiv v1 v2 = case (v1, v2) of
    (_, IntValue 0) -> Left $ RuntimeError DivisionByZero
    (_, FloatValue 0.0) -> Left $ RuntimeError DivisionByZero
    (IntValue i1, IntValue i2) -> 
        Right . FloatValue $ fromIntegral i1 / fromIntegral i2
    (IntValue i1, FloatValue f2) -> 
        Right . FloatValue $ fromIntegral i1 / f2
    (FloatValue f1, IntValue i2) -> 
        Right . FloatValue $ f1 / fromIntegral i2
    (FloatValue f1, FloatValue f2) -> 
        Right . FloatValue $ f1 / f2
    _ -> Left . RuntimeError $ typeMismatch "number" v1

-- | Arithmetic operations
execAdd :: InterpreterState -> Either InterpError InterpreterState
execAdd = binaryArithmetic (+)

execSub :: InterpreterState -> Either InterpError InterpreterState
execSub = binaryArithmetic (-)

execMul :: InterpreterState -> Either InterpError InterpreterState
execMul = binaryArithmetic (*)