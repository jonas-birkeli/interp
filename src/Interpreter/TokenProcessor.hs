module Interpreter.TokenProcessor
    (
        -- Core functions
        executeProgram,
        executeOperator,
        extractFinalValue,
        evaluateValue,
        evaluateList,

        -- Token execution
        splitAtQuotation,
        executeToken,
        executeValue,
        executeQuotationOrValue,
        executeExec,

        -- Control flow
        executeIf,
        executeTimes,
        executeLoop,
        executeLoopWithComponents,
        execTimesIterative,

        -- Higher order functions
        applyEach,
        executeMap,
        executeEach,
        executeFoldl,
        applyToItem,
        applyFunction,
        applyToElement,
        mapListWithQuotation,
        foldListWithQuotation,
    ) where

import Types 
import Interpreter.Stack
import qualified Data.Map as Map
import Interpreter.Variables
import Interpreter.String
import Interpreter.List
import Interpreter.IO
import Interpreter.Comparison
import Interpreter.Arithmetic
import Control.Monad

-- | Split token stream at the next quotation or single value
splitAtQuotation :: [Token] -> Either ProgramError ([Token], [Token])
splitAtQuotation [] = Left $ UnknownSymbol "Expected quotation, found end of program"
splitAtQuotation (ValueToken (QuotationValue tokens):rest) = Right (tokens, rest)
splitAtQuotation (token:rest) = Right ([token], rest)

executeToken :: [Token] -> State -> Either ProgramError (State, [Token])
executeToken [] state = Right (state, [])
executeToken (token:tokens) state = case token of
    -- Control flow tokens with access to tokens
    IfToken          -> executeAndContinue executeIf tokens state
    TimesToken       -> executeAndContinue executeTimes tokens state
    LoopToken        -> executeAndContinue executeLoop tokens state
    MapToken         -> executeAndContinue executeMap tokens state
    EachToken        -> executeAndContinue executeEach tokens state
    FoldlToken       -> executeAndContinue executeFoldl tokens state

    -- Standard execution
    ValueToken value -> (\newState -> (newState, tokens)) <$> executeValue value state
    OperatorToken op -> (\newState -> (newState, tokens)) <$> executeOperator op state
    AssignmentToken  -> (\newState -> (newState, tokens)) <$> executeAssignment state
    FunctionToken    -> (\newState -> (newState, tokens)) <$> executeFunction state
    ExecToken        -> (\newState -> (newState, tokens)) <$> executeExec state

-- | Helper function to execute a token and continue with remaining tokens
executeAndContinue :: ([Token] -> State -> Either ProgramError (State, [Token]))
                   -> [Token]
                   -> State
                   -> Either ProgramError (State, [Token])
executeAndContinue operation tokens state = do
    (state', remainingTokens) <- operation tokens state
    executeToken remainingTokens state'

-- | Execute a value, handling symbol lookup
executeValue :: Value -> State -> Either ProgramError State
executeValue value s = case value of
    SymbolValue name ->
        -- Check for occurancei in dict
        case Map.lookup name (dictionary s) of
            Just quotation@(QuotationValue _) ->
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
        executeToken tokens state >>= \(finalState, _) -> Right finalState
    -- For non-quotation values, just push onto the stack
    _ -> Right $ pushValue value state

-- | Execute exec operation
executeExec :: State -> Either ProgramError State
executeExec state = do
    (quotation, state') <- popValue state
    case quotation of
        QuotationValue tokens -> do
            (finalState, _) <- executeToken tokens state'
            return finalState
        _ -> Left $ ExpectedQuotation quotation

-- | Execute if operation - handles the token stream
executeIf :: [Token] -> State -> Either ProgramError (State, [Token])
executeIf tokens state = do
    (condition, state') <- popValue state
    (thenBranch, afterThen) <- splitAtQuotation tokens
    (elseBranch, rest) <- splitAtQuotation afterThen
    
    case condition of
        BoolValue True -> do
            (finalState, _) <- executeToken thenBranch state'
            return (finalState, rest)
        BoolValue False -> do
            (finalState, _) <- executeToken elseBranch state'
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
            (condState, _) <- executeToken condTokens currentState
            case stack condState of
                (BoolValue True:restStack) ->
                    -- Condition is true, exit the loop
                    Right $ condState { stack = restStack }
                (BoolValue False:restStack) -> do
                    -- Condition is false, execute body and continue
                    let stateAfterCond = condState { stack = restStack }
                    (bodyState, _) <- executeToken bodyTokens stateAfterCond
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
            (s', _) <- executeToken tokens s
            loop (n-1) s'
    in loop count state

-- | Apply each operation to a single item
applyEach :: Value -> [Token] -> State -> Either ProgramError State
applyEach item tokens state = do
    let state' = pushValue item state
    (resultState, _) <- executeToken tokens state'
    return resultState

-- | Execute each operation
executeEach :: [Token] -> State -> Either ProgramError (State, [Token])
executeEach [] state = Right (state, [])
executeEach tokens state = do
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

-- The foldl operation takes a list, an initial accumulator, and a quotation
-- and folds the list from left to right, applying the quotation to each element.
--
-- Examples:
--
-- >>> executeTokenStream (parseTokens' "[ 5 3 2 ] 1 foldl { * }") initialState
-- Right (State {dictionary = fromList [], stack = [30], printBuffer = []},[] )
--
-- >>> executeTokenStream (parseTokens' "[ 1 2 3 ] 0 foldl { + }") initialState
-- Right (State {dictionary = fromList [], stack = [6], printBuffer = []},[] )
--
-- >>> executeTokenStream (parseTokens' "[ \"a\" \"b\" \"c\" ] \"\" foldl { swap + }") initialState
-- Right (State {dictionary = fromList [], stack = ["abc"], printBuffer = []},[] )
executeFoldl :: [Token] -> State -> Either ProgramError (State, [Token])
executeFoldl [] _ = Left $ UnknownSymbol "Expected quotation, found end of program"
executeFoldl tokens state = do
    (quotationTokens, remainingTokens) <- splitAtQuotation tokens
    (initial, state1) <- popValue state
    (list, state2) <- popValue state1
    case list of
        ListValue values -> do
            -- Run the fold op with the list, initial value and quotation
            finalState <- foldListWithQuotation values initial quotationTokens state2
            return (finalState, remainingTokens)
        _ -> Left $ ExpectedList list

-- | Apply function to a single item
applyToItem :: [Token] -> State -> Value -> Either ProgramError Value
applyToItem tokens state item = do
    -- Push item onto a clean state (starting with original state but empty stack)
    let itemState = pushValue item state { stack = [] }
    -- Execute the tokens on this state
    (resultState, _) <- executeToken tokens itemState
    -- Get the result from the top of the stack
    case stack resultState of
        (result:_) -> Right result
        [] -> Left StackEmpty

-- | Apply a function to a single value
applyFunction :: [Token] -> Value -> Either ProgramError Value
applyFunction tokens value = do
    -- Duplicate state to make a copy of the stack
    let tempState = State { 
            dictionary = Map.empty, 
            stack = [value],
            printBuffer = []
        }
    (resultState, _) <- executeToken tokens tempState
    case stack resultState of
        (result:_) -> Right result
        [] -> Left StackEmpty

-- | Apply a function to a single element and update the state
applyToElement :: [Token] -> State -> Value -> Either ProgramError State
applyToElement tokens state element = do
    let stateWithElement = pushValue element state
    -- Execute the tokens in new state
    (resultState, _) <- executeToken tokens stateWithElement
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
    foldM foldItem (state, initial) values >>= \(finalState, result) ->
        return $ pushValue result finalState
    where 
        foldItem :: (State, Value) -> Value -> Either ProgramError (State, Value)
        foldItem (currentState, acc) item = do
            -- psuh acc and current item onto stack in correct order
            let stateWithValues = pushValue item $ pushValue acc currentState

            (newState, _) <- executeToken tokens stateWithValues

            case stack newState of
                (newAcc:restStack) ->
                    Right (newState { stack = restStack }, newAcc)
                [] ->
                    Left StackEmpty

-- | Execute the program (list of tokens) with given state
executeProgram :: [Token] -> State -> Either ProgramError (State, Value)
executeProgram tokens state = do
    (finalState, _) <- executeToken tokens state
    (finalState', value) <- extractFinalValue finalState
    let evaluatedValue = evaluateValue value finalState'
    return (finalState, evaluatedValue)

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
    _ -> \_ -> Left $ UnknownSymbol op

-- | Extract the final value from state
extractFinalValue :: State -> Either ProgramError (State, Value)
extractFinalValue state = case stack state of
    [] -> Right (state, SymbolValue "")  -- Return a dummy empty symbol instead of an error
    [v] -> Right (state, v)
    vs -> Left (ProgramFinishedWithMultipleValues vs)

-- | Evaluate a value, resolving symbols in lists
evaluateValue :: Value -> State -> Value
evaluateValue (ListValue lv) state = ListValue (evaluateList lv state)
evaluateValue value _ = value

-- | Evaluate a list by replacing symbols with their values
evaluateList :: [Value] -> State -> [Value]
evaluateList [] _ = []
evaluateList (SymbolValue name : rest) state =
    case Map.lookup name (dictionary state) of
        Just value -> value : evaluateList rest state
        Nothing -> SymbolValue name : evaluateList rest state
evaluateList (value : rest) state = value : evaluateList rest state