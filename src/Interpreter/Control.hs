module Interpreter.Control
    (
        executeIf,
        executeTimes,
        executeLoop,
        executeLoopWithComponents,
        execTimesIterative
    ) where

import Types (ProgramError(..), State(..), Value(..))
import Interpreter.Stack (pushValue, popValue)
import Interpreter.Core (splitAtQuotation, executeTokenStream)

-- | Execute if operation - handles the token stream
executeIf :: [Token] -> State -> Either ProgramError (State, [Token])
executeIf tokens state = do
    (condition, state') <- popValue state
    (thenBranch, afterThen) <- splitAtQuotation tokens
    (elseBranch, rest) <- splitAtQuotation afterThen
    
    case condition of
        BoolValue True -> do
            (finalState, _) <- executeTokenStream thenBranch state'
            return (finalState, rest)
        BoolValue False -> do
            (finalState, _) <- executeTokenStream elseBranch state'
            return (finalState, rest)
        _ -> Left $ ExpectedBool condition

-- | Execute times operation - handles the token stream
executeTimes :: [Token] -> State -> Either ProgramError (State, [Token])
executeTimes tokens state = do
    -- Pop the count and get the code block
    (countValue, state') <- popValue state
    (block, rest) <- splitAtQuotation tokens
    
    case countValue of
        IntValue count ->
            if count <= 0
                then Right (state', rest)
                else do
                    finalState <- execTimesIterative block count state'
                    return (finalState, rest)
        _ -> Left $ ExpectedBoolOrNumber countValue

-- | Execute loop operation - handles the token stream
executeLoop :: [Token] -> State -> Either ProgramError (State, [Token])
executeLoop tokens state = do
    -- Pop the initial value and get break condition and body from token stream
    (initial, state') <- popValue state
    (condTokens, afterCond) <- splitAtQuotation tokens
    (bodyTokens, rest) <- splitAtQuotation afterCond
    
    -- Execute the loop with these components
    finalState <- executeLoopWithComponents initial condTokens bodyTokens state'
    return (finalState, rest)

-- | Execute a loop with specified components
executeLoopWithComponents :: Value -> [Token] -> [Token] -> State -> Either ProgramError State
executeLoopWithComponents initial condTokens bodyTokens initState =
    let initialState' = pushValue initial initState
        loop currentState = do
            -- Execute the condition and check top of stack
            (condState, _) <- executeTokenStream condTokens currentState
            case stack condState of
                (BoolValue True:restStack) ->
                    -- Condition is true, exit the loop
                    Right $ condState { stack = restStack }
                (BoolValue False:restStack) -> do
                    -- Condition is false, execute body and continue
                    let stateAfterCond = condState { stack = restStack }
                    (bodyState, _) <- executeTokenStream bodyTokens stateAfterCond
                    loop bodyState
                (invalidValue:_) ->
                    Left (ExpectedBool invalidValue)
                [] ->
                    Left StackEmpty
    in loop initialState'

-- | Iterative implementation of times operation
execTimesIterative :: [Token] -> Integer -> State -> Either ProgramError State
execTimesIterative tokens count state =
    let loop 0 s = Right s
        loop n s = do
            (s', _) <- executeTokenStream tokens s
            loop (n-1) s'
    in loop count state