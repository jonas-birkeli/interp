module Interp.Eval.Higher(
   execMap,
   execEach,
   execFoldl
) where

import Interp.Core.Types
import Interp.Core.Error
import Interp.Core.State
import Interp.Eval.Stack

-- | Execute map operation
--
-- >>> let state = initialState { program = [LiteralToken (QuotationValue [OperatorToken "dup"])] }
-- >>> let state' = push (ListValue [IntValue 1, IntValue 2]) state
-- >>> fmap program (execMap state')
-- Right [LiteralToken (IntValue 1),OperatorToken "dup",LiteralToken (IntValue 2),OperatorToken "dup",LiteralToken (ListValue []),OperatorToken "cons",OperatorToken "cons"]
--
execMap :: InterpreterState -> Either InterpError InterpreterState
execMap state = case program state of
    (block : rest) ->
        pop state >>= \(v, state') -> case v of
            ListValue items ->
                let blockTokens = expandBlock block
                    mapProgram = concatMap (\item -> LiteralToken item : blockTokens) items ++
                                 [LiteralToken (ListValue [])] ++
                                 replicate (length items) (OperatorToken "cons")
                in Right state' { program = mapProgram ++ rest }
            _ -> Left . RuntimeError $ typeMismatch "list" v
    _ -> Left . RuntimeError $ typeMismatch "map with block" (QuotationValue [])

-- | Execute each operation
--
-- >>> let state = initialState { program = [LiteralToken (QuotationValue [OperatorToken "print"])] }
-- >>> let state' = push (ListValue [IntValue 1, IntValue 2]) state
-- >>> fmap program (execEach state')
-- Right [LiteralToken (IntValue 1),OperatorToken "print",LiteralToken (IntValue 2),OperatorToken "print"]
--
execEach :: InterpreterState -> Either InterpError InterpreterState
execEach state = case program state of
    (block : rest) ->
        pop state >>= \(v, state') -> case v of
            ListValue items ->
                let blockTokens = expandBlock block
                    eachProgram = concatMap (\item -> LiteralToken item : blockTokens) items
                in Right state' { program = eachProgram ++ rest }
            _ -> Left . RuntimeError $ typeMismatch "list" v
    _ -> Left . RuntimeError $ typeMismatch "each with block" (QuotationValue [])

-- | Execute foldl operation
--
-- >>> let state = initialState { program = [LiteralToken (QuotationValue [OperatorToken "+"])] }
-- >>> let state' = push (IntValue 0) $ push (ListValue [IntValue 1, IntValue 2, IntValue 3]) state
-- >>> fmap program (execFoldl state')
-- Right [LiteralToken (IntValue 0),LiteralToken (IntValue 1),OperatorToken "+",LiteralToken (IntValue 2),OperatorToken "+",LiteralToken (IntValue 3),OperatorToken "+"]
--
execFoldl :: InterpreterState -> Either InterpError InterpreterState
execFoldl state = case program state of
    (block : rest) ->
        pop state >>= \(initial, state1) ->
        pop state1 >>= \(listVal, state2) -> case listVal of
            ListValue items ->
                let blockTokens = expandBlock block
                    foldProgram = LiteralToken initial : 
                                  concatMap (\item -> LiteralToken item : blockTokens) items
                in Right state2 { program = foldProgram ++ rest }
            _ -> Left . RuntimeError $ typeMismatch "list" listVal
    _ -> Left . RuntimeError $ typeMismatch "foldl with block" (QuotationValue [])

-- | Expand a block (quotation or single value)
expandBlock :: Token -> [Token]
expandBlock (LiteralToken (QuotationValue tokens)) = tokens
expandBlock token = [token]