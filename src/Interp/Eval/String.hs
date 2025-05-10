module Interp.Eval.String(
   execParseInteger,
   execParseFloat,
   execWords
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Core.State
import Interp.Eval.Stack
import Text.Read (readMaybe)

-- | Execute parseInteger operation
--
-- >>> let state = push (StringValue "42") initialState
-- >>> fmap stack (execParseInteger state)
-- Right [IntValue 42]
--
-- >>> let state' = push (StringValue "abc") initialState
-- >>> execParseInteger state'
-- Left (RuntimeError (ConversionError "abc" "integer"))
--
execParseInteger :: InterpreterState -> Either InterpError InterpreterState
execParseInteger state = pop state >>= \(v, state') -> case v of
    StringValue s -> case readMaybe s of
        Just i -> Right $ push (IntValue i) state'
        Nothing -> Left . RuntimeError $ conversionError s "integer"
    _ -> Left . RuntimeError $ typeMismatch "string" v

-- | Execute parseFloat operation
--
-- >>> let state = push (StringValue "3.14") initialState
-- >>> fmap stack (execParseFloat state)
-- Right [FloatValue 3.14]
--
-- >>> let state' = push (StringValue "abc") initialState
-- >>> execParseFloat state'
-- Left (RuntimeError (ConversionError "abc" "float"))
--
execParseFloat :: InterpreterState -> Either InterpError InterpreterState
execParseFloat state = pop state >>= \(v, state') -> case v of
    StringValue s -> case readMaybe s of
        Just f -> Right $ push (FloatValue f) state'
        Nothing -> Left . RuntimeError $ conversionError s "float"
    _ -> Left . RuntimeError $ typeMismatch "string" v

-- | Execute words operation
--
-- >>> let state = push (StringValue "hello world test") initialState
-- >>> fmap stack (execWords state)
-- Right [ListValue [StringValue "hello",StringValue "world",StringValue "test"]]
--
-- >>> let state' = push (StringValue "") initialState
-- >>> fmap stack (execWords state')
-- Right [ListValue []]
--
-- >>> let state'' = push (StringValue "adam bob charlie") initialState  
-- >>> fmap stack (execWords state'')
-- Right [ListValue [StringValue "adam",StringValue "bob",StringValue "charlie"]]
--
execWords :: InterpreterState -> Either InterpError InterpreterState
execWords state = pop state >>= \(v, state') -> case v of
    StringValue s -> 
        let wordList = map StringValue (words s)
        in Right $ push (ListValue wordList) state'
    _ -> Left . RuntimeError $ typeMismatch "string" v