module Interp.Core.Error(
    InterpError(..),
    ParseError(..),
    RuntimeError(..),
    
    typeMismatch,
    stackUnderflow,
    conversionError
) where

import Interp.Core.Types

-- | Main error type
data InterpError
    = ParseError ParseError
    | RuntimeError RuntimeError
    deriving (Eq, Show)

-- | Parser errors
data ParseError
    = IncompleteString String
    | IncompleteList String
    | IncompleteQuotation String
    | UnexpectedEnd Char
    | UnknownToken String
    deriving (Eq, Show)

-- | Runtime errors
data RuntimeError
    = StackUnderflow
    | TypeMismatch String Value
    | DivisionByZero
    | SymbolNotFound String
    | ProgramFinishedWithMultipleValues [Value]
    | ProgramFinishedWithNoValues
    | ConversionError String String
    deriving (Eq, Show)

-- | Create a type mismatch error
typeMismatch :: String -> Value -> RuntimeError
typeMismatch = TypeMismatch

-- | Create a stack underflow error for n required items
stackUnderflow :: Int -> RuntimeError
stackUnderflow _ = StackUnderflow

-- | Create a conversion error
conversionError :: String -> String -> RuntimeError
conversionError = ConversionError