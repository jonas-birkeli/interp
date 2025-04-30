module Interpreter.String
    (
        executeParseInteger,
        executeParseFloat,
        executeWords
    ) where

import Types
import Interpreter.Stack
import Text.Read

-- | Execute parseInteger operation
executeParseInteger :: State -> Either ProgramError State
executeParseInteger state = do
    (value, state') <- popValue state
    case value of
        StringValue s ->
            maybe
                (Left $ NumberConversionError s)
                (\i -> Right $ pushValue (IntValue i) state')
                (readMaybe s)
        _ -> Left $ ExpectedEnumerable value

-- | Execute parseFloat operation
executeParseFloat :: State -> Either ProgramError State
executeParseFloat state = do
    (value, state') <- popValue state
    case value of
        StringValue s ->
            maybe
                (Left $ NumberConversionError s)
                (\f -> Right $ pushValue (FloatValue f) state')
                (readMaybe s)
        _ -> Left $ ExpectedEnumerable value

-- | Execute words operation
executeWords :: State -> Either ProgramError State
executeWords state = do
    (value, state') <- popValue state
    case value of
        StringValue s ->
            let wordList = ListValue $ map StringValue (words s)
            in Right $ pushValue wordList state'
        _ -> Left (ExpectedEnumerable value)