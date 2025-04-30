module Interpreter.List
    (
        executeHead,
        executeTail,
        executeEmpty,
        executeLength,
        executeCons,
        executeAppend
    ) where

import Types
import Interpreter.Stack

-- | Execute head operation
executeHead :: State -> Either ProgramError State
executeHead state = do
    (value, state') <- popValue state
    case value of
        ListValue [] -> Left (ExpectedList value)
        ListValue (x:_) -> Right $ pushValue x state'
        _ -> Left (ExpectedList value)

-- | Execute tail operation
executeTail :: State -> Either ProgramError State
executeTail state = do
    (value, state') <- popValue state
    case value of
        ListValue [] -> Left (ExpectedList value)
        ListValue (_:xs) -> Right $ pushValue (ListValue xs) state'
        _ -> Left (ExpectedList value)

-- | Execute empty operation
executeEmpty :: State -> Either ProgramError State
executeEmpty state = do
    (value, state') <- popValue state
    case value of
        ListValue [] -> Right $ pushValue (BoolValue True) state'
        ListValue _ -> Right $ pushValue (BoolValue False) state'
        _ -> Left $ ExpectedList value

-- | Execute length operation
executeLength :: State -> Either ProgramError State
executeLength state = do
    (value, state') <- popValue state
    case value of
        ListValue lv -> 
            Right $ pushValue (IntValue $ fromIntegral $ length lv) state'
        StringValue sv -> 
            Right $ pushValue (IntValue $ fromIntegral $ length sv) state'
        QuotationValue qv -> 
            Right $ pushValue (IntValue $ fromIntegral $ length qv) state'
        _ -> Left $ ExpectedEnumerable value

-- | Execute cons operation
executeCons :: State -> Either ProgramError State
executeCons state = do
    (list, state1) <- popValue state
    (item, state2) <- popValue state1
    case list of
        ListValue xs -> Right $ pushValue (ListValue (item:xs)) state2
        _ -> Left $ ExpectedList list

-- | Execute append operation
executeAppend :: State -> Either ProgramError State
executeAppend state = do
    (list1, state1) <- popValue state
    (list2, state2) <- popValue state1
    case (list1, list2) of
        (ListValue xs, ListValue ys) -> Right $ pushValue (ListValue (ys ++ xs)) state2
        _ -> Left $ ExpectedList list2