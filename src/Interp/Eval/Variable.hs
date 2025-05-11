module Interp.Eval.Variable(
   execAssign,
   execFunDef,
   execSymbol
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Eval.Stack
import qualified Data.Map as Map

-- | Execute assignment operation
--
-- >>> let state = push (IntValue 42) $ push (SymbolValue "x") initialState
-- >>> case execAssign state of Right s -> Map.lookup "x" (dict s); _ -> Nothing
-- Just (IntValue 42)
--
-- >>> let state' = push (IntValue 42) $ push (IntValue 10) initialState
-- >>> execAssign state'
-- Left (RuntimeError (TypeMismatch "symbol" (IntValue 10)))
--
execAssign :: InterpreterState -> Either InterpError InterpreterState
execAssign state = pop state >>= \(value, state1) -> 
    pop state1 >>= \(symbol, state2) -> case symbol of
        SymbolValue name -> Right state2 { dict = Map.insert name value (dict state2) }
        _ -> Left . RuntimeError $ typeMismatch "symbol" symbol

-- | Execute function definition
--
-- >>> let state = push (QuotationValue [LiteralToken (IntValue 1), OperatorToken "+"]) $ push (SymbolValue "inc") initialState
-- >>> case execFunDef state of Right s -> Map.lookup "inc" (dict s); _ -> Nothing
-- Just (QuotationValue [LiteralToken (IntValue 1),OperatorToken "+"])
--
-- >>> let state' = push (IntValue 42) $ push (SymbolValue "f") initialState
-- >>> execFunDef state'
-- Left (RuntimeError (TypeMismatch "quotation" (IntValue 42)))
--
execFunDef :: InterpreterState -> Either InterpError InterpreterState
execFunDef state = pop state >>= \(quotation, state1) -> 
    pop state1 >>= \(symbol, state2) -> case (symbol, quotation) of
        (SymbolValue name, q@(QuotationValue _)) -> 
            Right state2 { dict = Map.insert name q (dict state2) }
        (SymbolValue _, v) -> Left . RuntimeError $ typeMismatch "quotation" v
        (v, _) -> Left . RuntimeError $ typeMismatch "symbol" v

-- | Execute a symbol by looking it up and evaluating
--
-- >>> let state = initialState { dict = Map.singleton "x" (IntValue 42) }
-- >>> fmap stack (execSymbol "x" state)
-- Right [IntValue 42]
--
-- >>> let state' = initialState { dict = Map.singleton "inc" (QuotationValue [LiteralToken (IntValue 1), OperatorToken "+"]) }
-- >>> fmap program (execSymbol "inc" state')
-- Right [LiteralToken (IntValue 1),OperatorToken "+"]
--
-- >>> fmap stack (execSymbol "unknown" initialState)
-- Right [SymbolValue "unknown"]
--
execSymbol :: String -> InterpreterState -> Either InterpError InterpreterState
execSymbol name state = case Map.lookup name (dict state) of
    Just (QuotationValue tokens) -> Right state { program = tokens ++ program state }
    Just value -> Right $ push value state
    Nothing -> Right $ push (SymbolValue name) state