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




-- | Push a value onto the stack
pushValue :: Value -> State -> State
pushValue value state = state { stack = value : stack state }