module Interp.Eval.Stack(
   push,
   pop,
   popN,
   peek,
   execDup,
   execSwap,
   execPop,
   hasNValues
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Core.State

-- | Push a value onto the stack
--
-- >>> let state = initialState
-- >>> stack (push (IntValue 42) state)
-- [IntValue 42]
--
push :: Value -> InterpreterState -> InterpreterState
push v state = state { stack = v : stack state }

-- | Pop a value from the stack
--
-- >>> let state = push (IntValue 42) initialState
-- >>> fmap (stack . snd) (pop state)
-- Right []
--
-- >>> pop initialState
-- Left (RuntimeError StackUnderflow)
--
pop :: InterpreterState -> Either InterpError (Value, InterpreterState)
pop state = case stack state of
    [] -> Left $ RuntimeError StackUnderflow
    (v:vs) -> Right (v, state { stack = vs })

-- | Pop multiple values from the stack
--
-- >>> let state = push (IntValue 3) $ push (IntValue 2) $ push (IntValue 1) initialState
-- >>> fmap fst (popN 2 state)
-- Right [IntValue 1,IntValue 2]
--
-- >>> popN 5 state
-- Left (RuntimeError StackUnderflow)
--
popN :: Int -> InterpreterState -> Either InterpError ([Value], InterpreterState)
popN n = go n []
  where
    go 0 acc s = Right (acc, s)
    go i acc s = pop s >>= \(v, s') -> go (i-1) (acc ++ [v]) s'

-- | Peek at the top value without popping
--
-- >>> let state = push (IntValue 42) initialState
-- >>> peek state
-- Right (IntValue 42)
--
-- >>> peek initialState
-- Left (RuntimeError StackUnderflow)
--
peek :: InterpreterState -> Either InterpError Value
peek state = case stack state of
    [] -> Left $ RuntimeError StackUnderflow
    (v:_) -> Right v

-- | Execute swap operation
--
-- >>> let state = push (IntValue 20) $ push (IntValue 10) initialState
-- >>> fmap stack (execSwap state)
-- Right [IntValue 20,IntValue 10]
--
execSwap :: InterpreterState -> Either InterpError InterpreterState
execSwap state = pop state >>= \(v1, state1) ->
    pop state1 >>= \(v2, state2) ->
        Right $ push v2 $ push v1 state2

-- | Stack operation: dup (duplicate top value)
--
-- >>> let state = push (IntValue 42) initialState
-- >>> fmap stack (execDup state)
-- Right [IntValue 42,IntValue 42]
--
execDup :: InterpreterState -> Either InterpError InterpreterState
execDup state = pop state >>= \(v, state') -> 
    Right $ push v $ push v state'

-- | Stack operation: pop (discard top value)
--
-- >>> let state = push (IntValue 42) initialState
-- >>> fmap stack (execPop state)
-- Right []
--
execPop :: InterpreterState -> Either InterpError InterpreterState
execPop state = pop state >>= \(_, state') -> Right state'

-- | Check if stack has at least n values
--
-- >>> hasNValues 2 (push (IntValue 2) $ push (IntValue 1) initialState)
-- True
--
-- >>> hasNValues 3 (push (IntValue 1) initialState)
-- False
--
hasNValues :: Int -> InterpreterState -> Bool
hasNValues n state = length (stack state) >= n