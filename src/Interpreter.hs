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

-- | Execute length operation
executeLength :: State -> Either ProgramError State
executeLength state = do
    (value, state') <- popValue state
    case value of
        ListValue lv -> Right $ pushValue (IntValue $ fromIntegral $ length lv) state'
        StringValue sv -> Right $ pushValue (IntValue $ fromIntegral $ length sv) state'
        QuotationValue qv -> Right $ pushValue (IntValue $ fromIntegral $ length qv) state'
        _ -> Left $ ExpectedEnumerable value

-- | Execute cons operation
executeCons :: State -> Either ProgramError State
executeCons state = do
    (list, state1) <- popValue state
    (value, state2) <- popValue state1
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
    (sumbolValue, state2) <- popValue state1
    case symbolValue of
        sumbolValue name ->
            Right $ state2 { dictionary = Map.insert name value (dictionary state2) } -- Store value in dict
        _ -> Left $ ExpectedVariable symbolName

-- | Execute function definition operator (fun)
executeFunction :: State -> Either ProgramError State
executeFunction state = do
    (quotation, state1) <- popValue state
    (symbolValue, state2) <- popValue state1
    case (quotation, symbolValue) of
        (QuotationValue _, symbolValue name) -> 
            Right $ state2 { dictionary = Map.insert name quotation (dictionary state2) }
        (_, _) -> Left $ ExpectedQuotation quotation

-- | Execute if operation
executeIf :: State -> Either ProgramError State
executeIf state = do
    (elseBranch, state1) <- popValue state
    (thenBranch, state2) <- popValue state1
    (condition, state3) <- popValue state2
    case condition of
        BoolValue True -> executeQuotationOrValue thenBranch state3
        BoolValue False -> executeQuotationOrValue elseBranch state3
        _ -> Left $ ExpectedBool condition

-- | Execute a quotation or a direct value
executeQuotationOrValue :: Value -> State -> Either ProgramError State
executeQuotationOrValue value state = case value of
    QuotationValue tokens -> executeProgram tokens state >>= (\s, _) -> Right s
    _ -> Right $ pushValue value state

-- | Execute times operation
executeTimes :: State -> Either ProgramError State
executeTimes state = do
    (block, state1) <- popValue state
    (countValue, state2) <- popValue state1
    case countValue of
        IntValue count ->
            if count <= 0
                then Right state 2
                else case block of
                    QuotationValue tokens -> execTimesIterative tokens count state2
                    _ -> execTimesIterative [ValueToken block] count state2
        _ -> Left $ ExpectedBoolOrNumber

-- | Iterative implementation of times operation
execTimesIterative :: [Token] -> Integer -> State -> Either ProgramError State
execTimesIterative tokens count state =
    let loop 0 s = Right s
        loop n s = ExecuteProgram tokens s >>= \(s', _) -> loop (n-1) s'
    in loop count state

-- | Execute loop operation
executeLoop :: State -> Either ProgramError State
executeLoop state = do
    (body, state1) <- popValue state
    (condition, state2) <- popValue state1
    (initial, state3) <- popValue state2
    case (condition, body) of
        (QuotationValue condTokens, QuotationValue bodyTokens) ->
            executeLoopWithInitial initial condTokens bodyTokens state3
        _ -> Left $ ExpectedQuotation conditional

-- | Execute a loop with an initial value
executeLoopWithInitial Value -> [Token] -> [Token] -> State -> Either ProgramError State
executeLoopWithInitial initial condTokens bodyTokens state =
    let state' = pushValue initial state
        loop s = do
            (condResult, s') <- executeProgram condTokens s
            case condResult of
                BoolValue True -> Right s'
                BoolValue False -> do
                    (s2, _) <- executeProgram bodyTokens s1
                    loop s2
                _ -> Left $ ExpectedBool condResult
    in loop state'

-- | Execute map operation
executeMap :: State -> Either ProgramError State
executeMap state = do
    (quotation, state1) <- popValue state
    (list, state2) <- popValue state1
    case (list, quotation) of
        (ListValue values, QuotationValue tokens) ->
            mapListWithQuotation values tokens state2
        (ListValue values, _) ->
            mapListWithQuotation values [ValueToken quotation] state2
        _ -> Left $ ExpectedList list

-- | Map list with a quotation
mapListWithQuotation :: [Value] -> [Token] -> State -> Either ProgramError State
mapListWithQuotation values tokens state = 
    let mapItem acc item = do
        state1 <- acc
        let state2 = pushValue item state1
        (resultState, result) <- executeProgram tokens state2
        return $ pushValue result $ resultState { stack = tail (stack resultState) }

        finalStateWithItems = foldl mapItem (Right state) values
    in finalStateWithItems >>= \s -> do
        items <- popValues (length values) s
        return $ pushValue (ListValue (reverse $ fst items)) (snd items)

-- | Execute each opearation
executeEach :: State -> Either ProgramError State
executeEach state = do
    (quotation, state1) <- popValue state
    (list, state2) <- popValue state1
    case (list, quotation) of
        (ListValue values, QuotationValue tokens) ->
            foldl (\s item -> s >>= applyEach item tokens) (Right state2) values
        (ListValue values, _) -> 
            foldl (\s item -> s >>= applyEach item [ValueToken quotation]) (Right state2) values
        _ -> Left $ ExpectedList list

-- | Apply each opeartion to a single item
applyEach :: Value -> [Token] -> State -> Either ProgramError State
applyEach item tokens state = do
    let state' = pushValue item state
    (resultState, _) <- executeProgram tokens state'
    return resultState

-- | Execute exec operation
executeExec :: State -> Either ProgramError State
executeExec state = do
    (quotation, state') <- popValue state
    case quotation of
        QuotationValue tokens -> executeProgram tokens state' >>= \(s, _) -> Right s
        _ -> Left $ ExpectedQuotation quotation
