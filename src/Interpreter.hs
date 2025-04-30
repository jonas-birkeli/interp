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
        stack = []
    }

-- | Execute the program (list of tokens) with given state
executeProgram :: [Token] -> State -> Either ProgramError (State, Value)
executeProgram tokens state = do
    (finalState, _) <- executeTokenStream tokens state
    extractFinalValue finalState

-- | Make executeTokenStream available for REPL mode
--executeTokenStream :: [Token] -> State -> Either ProgramError (State, [Token])

-- | Extract the final value from state
extractFinalValue :: State -> Either ProgramError (State, Value)
extractFinalValue state = case stack state of
    [] -> Left ProgramFinishedWithNoValues
    [v] -> Right (state, v)
    vs -> Left (ProgramFinishedWithMultipleValues vs)

-- | Execute a stream of tokens
executeTokenStream :: [Token] -> State -> Either ProgramError (State, [Token])
executeTokenStream [] state = Right (state, [])
executeTokenStream (token:tokens) state = 
    case token of
        -- Control flow operations that need to handle the token stream
        IfToken -> executeIf tokens state
        TimesToken -> executeTimes tokens state
        LoopToken -> executeLoop tokens state
        -- Defered functions, needs token stream
        MapToken -> executeMap tokens state
        EachToken -> executeEach tokens state
        -- For regular tokens, process them and continue with the stream
        _ -> do
            state' <- executeToken token state
            executeTokenStream tokens state'

-- | Execute a single token with a given state
executeToken :: Token -> State -> Either ProgramError State
executeToken token s = case token of
    ValueToken value -> executeValue value s
    OperatorToken op -> executeOperator op s
    AssignmentToken -> executeAssignment s
    FunctionToken -> executeFunction s
    --MapToken -> executeMap s
    FoldlToken -> executeFoldl s
    --EachToken -> executeEach s
    ExecToken -> executeExec s
    -- Control flow tokens should be handled by executeTokenStream
    IfToken -> Left $ UnknownSymbol "if token encountered out of context"
    TimesToken -> Left $ UnknownSymbol "times token encountered out of context"
    LoopToken -> Left $ UnknownSymbol "loop token encountered out of context"

-- | Execute a value, handling symbol lookup
executeValue :: Value -> State -> Either ProgramError State
executeValue value s = case value of
    SymbolValue name ->
        -- Check for occurancei in dict
        case Map.lookup name (dictionary s) of
            Just quotation@(QuotationValue tokens) ->
                executeQuotationOrValue quotation s
            Just value' ->
            -- Push the value if its not an quotation
                Right $ pushValue value' s
            Nothing ->
                -- Push the symbol itself if not found
                Right $ pushValue value s
    _ -> Right $ pushValue value s

-- | Execute a quotation or a direct value
executeQuotationOrValue :: Value -> State -> Either ProgramError State
executeQuotationOrValue value state = case value of
    QuotationValue tokens -> 
        -- Execute the quotation as a series of tokens
        executeTokenStream tokens state >>= \(finalState, _) -> Right finalState
    -- For non-quotation values, just push onto the stack
    _ -> Right $ pushValue value state

-- | Split token stream at the next quotation or single value
splitAtQuotation :: [Token] -> Either ProgramError ([Token], [Token])
splitAtQuotation [] = Left $ UnknownSymbol "Expected quotation, found end of program"
splitAtQuotation (ValueToken (QuotationValue tokens):rest) = Right (tokens, rest)
-- Handle single value as a 1-token quotation (as per spec, single-value branches don't need curly braces)
splitAtQuotation (token:rest) = Right ([token], rest)

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

-- | Iterative implementation of times operation
execTimesIterative :: [Token] -> Integer -> State -> Either ProgramError State
execTimesIterative tokens count state =
    let loop 0 s = Right s
        loop n s = do
            (s', _) <- executeTokenStream tokens s
            loop (n-1) s'
    in loop count state

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

-- | Execute integer division
executeIntegerDivision :: State -> Either ProgramError State
executeIntegerDivision state = do
    (v1, v2, state') <- popTwoValues state
    case (v1, v2) of
        (IntValue i1, IntValue i2) -> 
            if i2 == 0
                then Left DivisionByZero
                else Right $ pushValue (IntValue (i1 `div` i2)) state'
        _ -> Left $ ExpectedBoolOrNumber v1

-- | Execute floating-point division
executeFloatDivision :: State -> Either ProgramError State
executeFloatDivision state = do
    (v1, v2, state') <- popTwoValues state
    checkDivisionByZero v2
    result <- applyDivision (/) v1 v2
    return $ pushValue result state'

-- | Check for divison by zero
checkDivisionByZero :: Value -> Either ProgramError ()
checkDivisionByZero (IntValue 0) = Left DivisionByZero
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
    _ -> Left $ ExpectedBoolOrNumber v1

-- | Execute comparison operations
executeComparison :: (forall a. Ord a => a -> a -> Bool) -> State -> Either ProgramError State
executeComparison op state = do
    (v1, v2, state') <- popTwoValues state
    result <- applyComparison op v1 v2
    return $ pushValue (BoolValue result) state'

-- | Apply comparison operator to values
applyComparison :: (forall a. Ord a => a -> a -> Bool) -> Value -> Value -> Either ProgramError Bool
applyComparison op v1 v2 = case (v1, v2) of
    (IntValue i1, IntValue i2) -> Right $ op i1 i2
    (IntValue i1, FloatValue f2) -> Right $ op (fromIntegral i1) f2
    (FloatValue f1, IntValue i2) -> Right $ op f1 (fromIntegral i2)
    (FloatValue f1, FloatValue f2) -> Right $ op f1 f2
    _ -> Left $ ExpectedBoolOrNumber v1

-- | Execute equality operations
executeEquality :: State -> Either ProgramError State
executeEquality state = do
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
        _ -> Left $ ExpectedBoolOrNumber value

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
        ListValue lv -> Right $ pushValue (IntValue $ fromIntegral $ length lv) state'
        StringValue sv -> Right $ pushValue (IntValue $ fromIntegral $ length sv) state'
        QuotationValue qv -> Right $ pushValue (IntValue $ fromIntegral $ length qv) state'
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

-- | Execute exec operation
executeExec :: State -> Either ProgramError State
executeExec state = do
    (quotation, state') <- popValue state
    case quotation of
        QuotationValue tokens -> do
            (finalState, _) <- executeTokenStream tokens state'
            return finalState
        _ -> Left $ ExpectedQuotation quotation

-- | Execute print operation
executePrint :: State -> Either ProgramError State
executePrint state = do
    (value, state') <- popValue state
    -- Print to stdout? TODO
    return state'

-- | Execute read operation
executeRead :: State -> Either ProgramError State
executeRead state = do
    -- Get from stdin? TODO
    return $ pushValue (StringValue "") state

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

-- | Apply a function to a single value
applyFunction :: [Token] -> Value -> Either ProgramError Value
applyFunction tokens value = do
    -- Duplicate state to make a copy of the stack
    let tempState = State { dictionary = Map.empty, stack = [value]}
    (resultState, _) <- executeTokenStream tokens tempState
    case stack resultState of
        (result:_) -> Right result
        [] -> Left StackEmpty

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

-- | Apply a function to a single element and update the state
applyToElement :: [Token] -> State -> Value -> Either ProgramError State
applyToElement tokens state element = do
    let stateWithElement = pushValue element state
    -- Execute the tokens in new state
    (resultState, _) <- executeTokenStream tokens stateWithElement
    return resultState

-- | Apply each operation to a single item
applyEach :: Value -> [Token] -> State -> Either ProgramError State
applyEach item tokens state = do
    let state' = pushValue item state
    (resultState, _) <- executeTokenStream tokens state'
    return resultState

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

-- | Execute operator token
executeOperator :: String -> State -> Either ProgramError State
executeOperator op = case op of
    "+" -> executeArithmetic (+)
    "-" -> executeArithmetic (-)
    "*" -> executeArithmetic (*)
    "div" -> executeIntegerDivision
    "/" -> executeFloatDivision
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
    "parseFloat" -> executeParseFloat
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