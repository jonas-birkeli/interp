module Interpreter
    ( executeProgram
    , initialState
    ) where

import Types
import qualified Data.map as Map

-- | Initial interpreter state
initialState :: State
initialState = State
    { 
        dictionary = Map.empty,
        stack = []
    }

{-
-- | Execute the program (list of tokens) with given state
executeProgram :: [Token] -> State -> Either ProgramError (State, Value)
executeProgram tokens state = do
    finalState <- foldl (\stateResult token -> 
            case stateResult of
                Left err -> Left err
                Right st -> executeToken token st)
            (Right state)
            tokens
    case stack finalState of
        [] -> Left ProgramFinishedWithNoValues
        [v] -> Right (finalState, v)
        vs -> Left (ProgramFinishedWithMultipleValues vs)
        -}
        
        
-- | Execute the program (list of tokens) with given state
executeProgram :: [Token] -> State -> Either ProgramError (State, Value)
executeProgram tokens state = do
    finalSTate <- foldM executeToken state Tokens
    extractFinalValue finalState

-- | Extract the final value from state
extractFinalValue :: State -> Either ProgramError (State, Value)
extractFinalValue state = case stack state of
    [] -> Left ProgramFinishedWithNoValues
    [v] -> Right (state, v)
    vs -> Left (ProgramFinishedWithMultipleValues vs)

-- | Execute a single token with a given state
executeToken :: Token -> State -> Either ProgramError State
executeToken token = case token of
    ValueToken value -> Right (pushValue value)
    OperatorToken op -> executeOperator op
    AssignmentToken -> executeAssignment
    FunctionToken -> executeFunction
    IfToken -> executeIf
    TimesToken -> executeTimes
    LoopToken -> executeLoop
    MapToken -> executeMap
    FoldlToken -> executeFoldl
    EachToken -> executeEach
    ExecToken -> executeExec

-- | Execute an operator token
executeOperator :: String -> State -> Either ProgramError State
executeOperator op = case op of
    "+" -> executeArithmetic (+)
    "-" -> executeArithmetic (-)
    "*" -> executeArithmetic (*)
    "div" -> executeDivision div
    "/" -> executeDivision (/)
    "<" -> executeComparison (<)
    ">" -> executeComparison (>)
    "==" -> executeEquality
    "&&" -> executeLogical (&&)
    "||" -> executeLogical (||)
    "not" -> executeNot
    "dup" -> executeDup
    "swap" -> executeSwap
    "pop" -> executePop
    "parseInteger" -> executeParseInteger
    "parseFloat" -> executeparseFloat
    "words" -> executeWords
    "head" -> executeHead
    "tail" -> executeTail
    "empty" -> executeEmpty
    "length" -> executeLength
    "cons" -> executeCons
    "append" -> executeAppend
    "print" -> executePrint
    "read" -> executeRead
    _ -> \s -> Left $ UnknownSymbol op


-- | Push a value onto the stack
pushValue :: Value -> State -> State
pushValue value state = state { stack = value : stack state }

-- | Pop a value from the stack
popValue :: State -> Either ProgramError (Value, State)
popValue state = case stack state of
    [] -> Left StackEmpty
    (v:vs) -> Right (v, state { stack = vs })

-- | Pop multiple values from the stack
popValues :: Int -> State -> Either ProgramError ([Value], state)
popValues n state
    | n <= 0 = Right ([], state) -- Fallback
    | otherwise =
        let go 0 values s = Right (reverse values s) -- 0 = values to pop, values = accumulated popped values, s = current state
            go i values s = popValue s >>= \(v, s') -> go (i-1) (v:values) s'
            -- go 0 is base recursion end. (i-1) untill i = 0
        in go n [] state


-- | Pop two values from stack
popTwoValues :: State -> Either ProgramError (Value, Value, State)
popTwoValues state = popValues 2 state >>= \([v1, v2], state') -> Right (v1, v2, state') 

-- | Basic arithmetic operations
executeArithmetic :: (Double -> Double -> Double) -> State -> Either ProgramError State
executeArithmetic op state = 
    popTwoValues state >>= \(v1, v2, state') ->
        applyArithmetic op v1 v2 >>= \result ->
            Right (pushValue result state')

-- | Apply arithmetic operation to values
applyArithmetic :: (Double -> Double -> Double) -> Value -> Value -> Either ProgramError Value
applyArithmetic op v1 v2 = case (v1, v2) of
    (IntValue i1, IntValue i2) ->
        Right $ IntValue $ round $ op (fromIntegral i1) (fromIntegral i2)
    (IntValue i1, FloatValue f2) ->
        Right $ FloatValue $ op (fromIntegral i1) f2
    (FloatValue f1, IntValue i2) ->
        Right $ FloatValue $ op f1 (fromIntegral i2)
    (FloatValue f1, FloatValue f2) ->
        Right $ FloatValue $ op f1 f2
    _ -> Left $ ExpectedBoolOrNumber v1

-- | Divison operations
executeDivision :: (forall a. Fractional a => a -> a -> a) -> State -> Either ProgramError State
executeDivison op state = do
    (v1, v2, state') <- popTwoValues
    checkDivisionByZero v2
    result <- applyDivison op v1 v2
    return $ pushValue result state'

-- | Check for divison by zero
checkDivisionByZero :: Value -> Either ProgramError ()
checkDivisionByZero (Intvalue 0) = Left DivisionByZero
checkDivisionByZero (FloatValue 0.0) = Left DivisionByZero
checkDivisionByZero _ = Right ()

-- | Apply division to values
applyDivision :: (forall a. Fractional a => a -> a -> a) -> Value -> Value -> Either ProgramError Value
applyDivision op v1 v2 = case (v1, v2) of
    (IntValue i1, IntValue i2) -> 
        Right $ FloatValue $ op (fromIntegral i1) (fromIntegral i2)
    (IntValue i1, FloatValue f2) ->
        Right $ FloatValue $ op (fromIntegral i1) f2
    (FloatValue f1, IntValue i2) ->
        Right $ FloatValue $ op f1 (fromIntegral i2)
    (FloatValue f1, FloatValue f2) -> 
        Right $ FloatValue $ op f1 f2
    _ -> Left $ ExpectedBoolOrNumber v1 -- Division bv zero implicitly checks v2

-- | Execute comparison operations
executeComparison :: (forall a. Ord => a -> a -> bool) -> state -> Either ProgramError State
executeComparison op state = do
    (v1, v2, state') <- popTwoValues state
    result <- applyComparison op v1 v2
    return $ pushValue (BoolValue result) state'

-- | Apply comparison operator to values
applyComparison :: (forall a. Ord => a -> a -> bool) -> Value -> Value -> Either ProgramError Bool
applyComparison op v1 v2 = case (v1, v2) of
    (IntValue i1, IntValue i2) -> Right $ op i1 i2
    (IntValue i1, FloatValue f2) -> Right $ op (fromIntegral i1) f2
    (FloatValue f1, IntValue i2) -> Right $ op f1 (fromIntegral i2)
    (FloatValue f1, FloatValue f2) -> Right $ op f1 f2
    _ -> Left $ ExpectedBoolOrNumber v1

-- | Execute equality operations
executeEquality :: State -> Either ProgramError
    (v1, v2, state') <- popTwoValues state
    let result = v1 == v2
    return $ pushValue (BoolValue result) state'

-- | Execute logical opeartions
executeLogical :: (Bool -> Bool -> Bool) -> State -> Either ProgramError State
executeLogical op state = do
    (v1, v2, state') <- popTwoValues state
    result <- applyLogical op v1 v2
    return $ pushValue (BoolValue result) state'

-- | Apply logical operations to values
applyLogical :: (Bool -> Bool -> Bool) -> Value -> Value -> Either ProgramError Bool
applyLogical op (BoolValue b1) (BoolValue b2) = Right $ op b1 b2
applyLogical _ v1 _ = Left $ ExpectedBool v1

-- | Execute not operation
executeNot :: State -> Either ProgramError State
executeNot state = do
    (value, state') <- popValue state
    case value of
        BoolValue b -> return $ pushValue (BoolValue (not b)) state'
        IntValue i -> return $ pushValue (IntValue (-i)) state'
        _ -> Left $ ExpectedBoolNumber value

-- | Execute dup operation (duplicate top value)
executeDup :: State -> Either ProgramError State
executeDup state = do
    (value, _) <- popValue state
    return $ pushValue value $ pushValue value state

-- | Execute swap operations (swap top two values)
executeSwap :: State -> Either ProgramError State
executeSwap state = do
    (v1, state1) <- popValue state
    (v2, state2) <- popValue state1
    return $ pushValue v1 $ pushValue v2 state2

-- | Execute pop operation (remove top value)
executePop :: State -> Either ProgramError State
executePop = popValue >=> \(_, state') -> Right state'

-- | Execute parseInteger operation
executeParseInteger :: State -> Either ProgramError State
executeParseInteger state = do
    (value, state') <- popValue state
    case value of
        String s ->
            maybe
                (Left $ NumberConversionError s)
                (\i -> Right $ pushValue (IntValue i) state')
                (readMaybe s)
        _ -> Left $ ExpectedEnumerable

-- | Execute parseFloat operation
executeParseFloat :: State -> Either ProgramError State
executeParseFloat state = do
    (value, state') <- popValue state
    case value of
        String s ->
            maybe 
                (Left $ NumberConversionError s)
                (\f -> Right $ pushValue (FloatValue f) state')
        _ -> Left $ ExpectedEnumerable

-- | Execute words operation
executeWords :: State -> Either ProgramError State
executeWords state = do
    (value, state') <- popValue state
    case value of
        StringValue s ->
            let wordList = ListValue $ map (StringValue . show) (words s)
            in Right $ pushValue wordList state'
        _ -> Rigth $ ExpectedEnumerable value

-- | Execute head operation
executeHead :: State -> Either ProgramError State
executeHead state = do
    (value, state') <- popValue state
    case value of
        ListValue [] -> Left ExpectedList value
        ListValue (x:_) -> Right $ pushValue x state'
        _ -> Left ExpectedList value

-- | Execute tail operation
executeTail :: State -> Either ProgramError State
executeTail state = do
    (value, state') <- popValue state
    case value of 
        ListValue [] -> Left ExpectedList value
        ListValue (_:xs) -> Right $ psuhValue (ListValue xs) state'
        _ -> Left ExpectedList value

-- | Execute empty operation
executeEmpty :: State -> Either ProgramError State
executeEmpty state = do
    (value, state') <- popValue state
    case value of
        ListValue [] -> Right $ pushValue (BoolValue True) state'