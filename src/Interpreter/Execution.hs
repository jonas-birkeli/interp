module Interpreter.Execution 
    (
        executeTokenStream,
        splitAtQuotation
    ) where

import Types (Token(..), ProgramError(..), Value(..), State)

-- Declare but don't implement
executeToken :: Token -> State -> Either ProgramError State

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

-- | Split token stream at the next quotation or single value
splitAtQuotation :: [Token] -> Either ProgramError ([Token], [Token])
splitAtQuotation [] = Left $ UnknownSymbol "Expected quotation, found end of program"
splitAtQuotation (ValueToken (QuotationValue tokens):rest) = Right (tokens, rest)
splitAtQuotation (token:rest) = Right ([token], rest)