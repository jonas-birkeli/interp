{-# LANGUAGE RankNTypes #-}
module Interpreter.Comparison
    (
        executeComparison,
        executeEquality,
        executeLogical,
        executeNot,
        applyComparison,
        applyLogical
    ) where

import Types
import Interpreter.Stack

-- | Execute comparison operations
executeComparison :: (forall a. Ord a => a -> a -> Bool) -> State -> Either ProgramError State
executeComparison op state = do
    (v2, v1, state') <- popTwoValues state
    result <- applyComparison op v1 v2
    return $ pushValue (BoolValue result) state'

-- | Execute equality operations
executeEquality :: State -> Either ProgramError State
executeEquality state = do
    (v1, v2, state') <- popTwoValues state
    let result = case (v1, v2) of
                   (IntValue i1, FloatValue f2) -> fromIntegral i1 == f2
                   (FloatValue f1, IntValue i2) -> f1 == fromIntegral i2
                   _ -> v1 == v2
    return $ pushValue (BoolValue result) state'

-- | Execute logical opeartions
executeLogical :: (Bool -> Bool -> Bool) -> State -> Either ProgramError State
executeLogical op state = do
    (v1, v2, state') <- popTwoValues state
    result <- applyLogical op v1 v2
    return $ pushValue (BoolValue result) state'

-- | Execute not operation
executeNot :: State -> Either ProgramError State
executeNot state = do
    (value, state') <- popValue state
    case value of
        BoolValue b -> return $ pushValue (BoolValue (not b)) state'
        IntValue i -> return $ pushValue (IntValue (-i)) state'
        _ -> Left $ ExpectedBoolOrNumber value

-- | Apply comparison operator to values
applyComparison :: (forall a. Ord a => a -> a -> Bool) -> Value -> Value -> Either ProgramError Bool
applyComparison op v1 v2 = case (v1, v2) of
    (IntValue i1, IntValue i2) -> Right $ op i1 i2
    (IntValue i1, FloatValue f2) -> Right $ op (fromIntegral i1) f2
    (FloatValue f1, IntValue i2) -> Right $ op f1 (fromIntegral i2)
    (FloatValue f1, FloatValue f2) -> Right $ op f1 f2
    _ -> Left $ ExpectedBoolOrNumber v1

-- | Apply logical operations to values
applyLogical :: (Bool -> Bool -> Bool) -> Value -> Value -> Either ProgramError Bool
applyLogical op (BoolValue b1) (BoolValue b2) = Right $ op b1 b2
applyLogical _ v1 _ = Left $ ExpectedBool v1