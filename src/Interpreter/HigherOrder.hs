module Interpreter.HigherOrder
    ( 
        applyEach
        executeMap,
        executeEach,
        executeFoldl,
        applyToItem,
        applyFunction,
        applyToElement,
        mapListWithQuotation,
        foldListWithQuotation
    ) where

import Types (ProgramError(..), State(..), Value(..))
import Interpreter.Stack (pushValue, popValue)
import Interpreter.Core (splitAtQuotation, executeTokenStream)
import Control.Monad (foldM)

-- | Apply each operation to a single item
applyEach :: Value -> [Token] -> State -> Either ProgramError State
applyEach item tokens state = do
    let state' = pushValue item state
    (resultState, _) <- executeTokenStream tokens state'
    return resultState

-- | Execute each operation
executeEach :: [Token] -> State -> Either ProgramError (State, [Token])
--executeEach [] state = Right (state, [])
executeEach (token:tokens) state = do
    (quotationTokens, remainingTokens) <- splitAtQuotation tokens
    (listValue, state') <- popValue state
    case listValue of
        ListValue values -> do
            finalState <- foldM (applyToElement quotationTokens) state' values
            return (finalState, remainingTokens)
        _ -> Left $ ExpectedList listValue

-- | Execute map operation
executeMap :: [Token] -> State -> Either ProgramError (State, [Token])
executeMap tokens state = do
    (quotationTokens, remainingTokens) <- splitAtQuotation tokens
    (listValue, state') <- popValue state
    case listValue of
        ListValue values -> do
            -- Map over function, with function gotten from tokens
            result <- mapM (applyFunction quotationTokens) values
            let finalState = pushValue (ListValue result) state'
            return (finalState, remainingTokens)
        _ -> Left $ ExpectedList listValue

-- | Execute foldl operation
executeFoldl :: State -> Either ProgramError State
executeFoldl state = do
    (quotation, state1) <- popValue state
    (initial, state2) <- popValue state1
    (list, state3) <- popValue state2
    case list of
        ListValue values ->
            case quotation of
                QuotationValue tokens -> 
                    foldListWithQuotation values initial tokens state3
                _ -> 
                    foldListWithQuotation values initial [ValueToken quotation] state3
        _ -> Left $ ExpectedList list

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

-- | Apply a function to a single value
applyFunction :: [Token] -> Value -> Either ProgramError Value
applyFunction tokens value = do
    -- Duplicate state to make a copy of the stack
    let tempState = State { dictionary = Map.empty, stack = [value]}
    (resultState, _) <- executeTokenStream tokens tempState
    case stack resultState of
        (result:_) -> Right result
        [] -> Left StackEmpty

-- | Apply a function to a single element and update the state
applyToElement :: [Token] -> State -> Value -> Either ProgramError State
applyToElement tokens state element = do
    let stateWithElement = pushValue element state
    -- Execute the tokens in new state
    (resultState, _) <- executeTokenStream tokens stateWithElement
    return resultState

-- | Map a list with a quotation
mapListWithQuotation :: [Value] -> [Token] -> State -> Either ProgramError State
mapListWithQuotation values tokens state = do
    -- Process each value in the list, collecting results
    results <- mapM (applyToItem tokens state) values
    -- Push the list of results onto the stack
    return $ pushValue (ListValue results) state

-- | Fold a list with a quotation
foldListWithQuotation :: [Value] -> Value -> [Token] -> State -> Either ProgramError State
foldListWithQuotation values initial tokens state =
    let foldItem acc item = do
            (accValue, accState) <- acc
            let state' = pushValue item $ pushValue accValue accState
            (resultState, _) <- executeTokenStream tokens state'
            case stack resultState of
                (result:rest) -> Right (result, resultState { stack = rest })
                [] -> Left StackEmpty
    in foldM (\(acc, s) item -> foldItem (Right (acc, s)) item) (initial, state) values >>= \(result, finalState) ->
        return $ pushValue result finalState