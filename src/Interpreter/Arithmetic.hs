{-# LANGUAGE RankNTypes #-}
module Interpreter.Arithmetic
    (
        executeArithmetic,
        executeIntegerDivision,
        executeFloatDivision,
        applyArithmetic,
        applyDivision,
        checkDivisionByZero
    ) where

import Types
import Interpreter.Stack (pushValue, popTwoValues)

-- | Basic arithmetic operations
executeArithmetic :: (Double -> Double -> Double) -> State -> Either ProgramError State
executeArithmetic op state =
    popTwoValues state >>= \(v1, v2, state') ->
        applyArithmetic op v1 v2 >>= \result ->
            Right (pushValue result state')

-- | Execute integer division
executeIntegerDivision :: State -> Either ProgramError State
executeIntegerDivision state = do
    (v1, v2, state') <- popTwoValues state
    checkDivisionByZero v1
    case (v2, v1) of
        (IntValue i1, IntValue i2) -> 
            Right $ pushValue (IntValue (i1 `div` i2)) state'
        (IntValue i1, FloatValue f2) ->
            Right $ pushValue (IntValue (i1 `div` round f2)) state'
        (FloatValue f1, IntValue i2) ->
            Right $ pushValue (IntValue (round f1 `div` i2)) state'
        (FloatValue f1, FloatValue f2) ->
            Right $ pushValue (IntValue (round f1 `div` round f2)) state'
        _ -> Left $ ExpectedBoolOrNumber v1

-- | Execute floating-point division
executeFloatDivision :: State -> Either ProgramError State
executeFloatDivision state = do
    (v1, v2, state') <- popTwoValues state
    checkDivisionByZero v1
    result <- applyDivision (/) v2 v1
    return $ pushValue result state'

-- | Apply arithmetic operation to values
applyArithmetic :: (Double -> Double -> Double) -> Value -> Value -> Either ProgramError Value
applyArithmetic op v1 v2 = case (v1, v2) of
    (IntValue i1, IntValue i2) ->
        Right $ IntValue $ round $ op (fromIntegral i1) (fromIntegral i2)
    (IntValue i1, FloatValue f2) ->
        Right $ FloatValue $ op (fromIntegral i1) f2
    (FloatValue f1, IntValue i2) ->
        Right $ FloatValue $ op f1 (fromIntegral i2)
    (FloatValue f1, FloatValue f2) ->
        Right $ FloatValue $ op f1 f2
    _ -> Left $ ExpectedBoolOrNumber v1

-- | Apply division to values
applyDivision :: (forall a. Fractional a => a -> a -> a) -> Value -> Value -> Either ProgramError Value
applyDivision op v1 v2 = case (v1, v2) of
    (IntValue i1, IntValue i2) ->
        Right $ FloatValue $ op (fromIntegral i1) (fromIntegral i2)
    (IntValue i1, FloatValue f2) ->
        Right $ FloatValue $ op (fromIntegral i1) f2
    (FloatValue f1, IntValue i2) ->
        Right $ FloatValue $ op f1 (fromIntegral i2)
    (FloatValue f1, FloatValue f2) ->
        Right $ FloatValue $ op f1 f2
    _ -> Left $ ExpectedBoolOrNumber v1

-- | Check for divison by zero
checkDivisionByZero :: Value -> Either ProgramError ()
checkDivisionByZero (IntValue 0) = Left DivisionByZero
checkDivisionByZero (FloatValue 0.0) = Left DivisionByZero
checkDivisionByZero _ = Right ()