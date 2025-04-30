{-# LANGUAGE RankNTypes #-}
module Interpreter
    ( executeProgram
    , initialState
    , executeTokenStream
    ) where

import Types (ProgramError(..), State(..), Token(..), Value(..))
import qualified Data.Map as Map
import Text.Read (readMaybe)
import Control.Monad ((>=>), foldM)

-- | Initial interpreter state
initialState :: State
initialState = State
    {
        dictionary = Map.empty,
        stack = [],
        printBuffer = []
    }



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

-- | Execute asisgnment operation (:=)
executeAssignment :: State -> Either ProgramError State
executeAssignment state = do
    (value, state1) <- popValue state
    (symbolValue, state2) <- popValue state1
    case symbolValue of
        SymbolValue name ->
            Right $ state2 { dictionary = Map.insert name value (dictionary state2) }
        _ -> Left $ ExpectedVariable symbolValue

-- | Execute function definition operator (fun)
executeFunction :: State -> Either ProgramError State
executeFunction state = do
    (quotation, state1) <- popValue state
    (symbolValue, state2) <- popValue state1
    case (quotation, symbolValue) of
        (QuotationValue _, SymbolValue name) ->
            Right $ state2 { dictionary = Map.insert name quotation (dictionary state2 )}
        (_, _) -> Left $ ExpectedQuotation quotation





-- | Map a list with a quotation
mapListWithQuotation :: [Value] -> [Token] -> State -> Either ProgramError State
mapListWithQuotation values tokens state = do
    -- Process each value in the list, collecting results
    results <- mapM (applyToItem tokens state) values
    -- Push the list of results onto the stack
    return $ pushValue (ListValue results) state

-- | Apply function to a single item
applyToItem :: [Token] -> State -> Value -> Either ProgramError Value
applyToItem tokens state item = do
    -- Push item onto a clean state (starting with original state but empty stack)
    let itemState = pushValue item state { stack = [] }
    -- Execute the tokens on this state
    (resultState, _) <- executeTokenStream tokens itemState
    -- Get the result from the top of the stack
    case stack resultState of
        (result:_) -> Right result
        [] -> Left StackEmpty





-- | Apply each operation to a single item
applyEach :: Value -> [Token] -> State -> Either ProgramError State
applyEach item tokens state = do
    let state' = pushValue item state
    (resultState, _) <- executeTokenStream tokens state'
    return resultState
