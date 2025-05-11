module Interp.Eval.IO(
    execPrint,
    execRead
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Eval.Stack

-- | Execute print operation by setting an interrupt
--
-- >>> let state = push (StringValue "hello") initialState
-- >>> case execPrint state of Right s -> case interrupt s of Just (PrintInterrupt msg) -> msg; _ -> ""; _ -> ""
-- "hello"
--
-- >>> let state' = push (IntValue 42) initialState
-- >>> case execPrint state' of Right s -> case interrupt s of Just (PrintInterrupt msg) -> msg; _ -> ""; _ -> ""
-- "42"
--
execPrint :: InterpreterState -> Either InterpError InterpreterState
execPrint state = pop state >>= \(v, state') ->
    let msg = case v of
            StringValue s -> s
            _ -> show v
    in Right state' { interrupt = Just (PrintInterrupt msg) }

-- | Execute read operation by setting an interrupt
--
-- >>> case execRead initialState of Right s -> case interrupt s of Just (ReadInterrupt _) -> True; _ -> False; _ -> False
-- True
--
execRead :: InterpreterState -> Either InterpError InterpreterState
execRead state = Right state { interrupt = Just (ReadInterrupt readContinuation) }
  where
    readContinuation :: String -> InterpreterState
    readContinuation input = push (StringValue input) state