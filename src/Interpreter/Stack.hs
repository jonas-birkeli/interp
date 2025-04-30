module Interpreter.Stack
    (
        pushValue,
        popValue,
        popValues,
        popTwoValues,
        executeDup,
        executeSwap,
        executePop
    ) where

import Types
import Control.Monad

-- | Push a value onto the stack
pushValue :: Value -> State -> State
pushValue value state = state { stack = value : stack state }

-- | Pop a value from the stack
popValue :: State -> Either ProgramError (Value, State)
popValue state = case stack state of
    [] -> Left StackEmpty
    (v:vs) -> Right (v, state { stack = vs })

-- | Pop multiple values from the stack
popValues :: Int -> State -> Either ProgramError ([Value], State)
popValues n state
    | n <= 0 = Right ([], state)
    | otherwise =
        let go 0 values s = Right (reverse values, s)
            go i values s = popValue s >>= \(v, s') -> go (i-1) (v:values) s'
        in go n [] state

-- | Pop two values from stack
popTwoValues :: State -> Either ProgramError (Value, Value, State)
popTwoValues state = popValues 2 state >>= \([v1, v2], state') -> Right (v1, v2, state')

-- | Execute dup operation (duplicate top value)
executeDup :: State -> Either ProgramError State
executeDup state = do
    (value, state') <- popValue state
    return $ pushValue value $ pushValue value state'

-- | Execute swap operations (swap top two values)
executeSwap :: State -> Either ProgramError State
executeSwap state = do
    (v1, state1) <- popValue state
    (v2, state2) <- popValue state1
    return $ pushValue v2 $ pushValue v1 state2

-- | Execute pop operation (remove top value)
executePop :: State -> Either ProgramError State
executePop = popValue >=> \(_, state') -> Right state'