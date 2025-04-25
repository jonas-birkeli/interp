module Types where

import qualified Data.Map as Map

-- | Core data types for values in the language
data Value
  = IntValue Integer
  | FloatValue Double
  | BoolValue Bool
  | StringValue String
  | ListValue [Value]
  | QuotationValue [Token]
  | SymbolValue Strign
  deriving (Eq)

-- | Custom show instance for Value
instance Show Value where
  show (IntValue i) = show i
  show (FloatValue f) = show f
  show (BoolValue b) = show b
  show (StringValue s) = "\"" ++ s ++ "\""
  show (ListValue lv) = show lv
  show (QuotationValue qv) = "{} " ++ unwords (map show qv) ++ " }"
  show (SymbolValue sv) = sv

-- | Tokens represent the parsed program
data Token
  = ValueToken Value
  | OperatorToken String
  | AssignmentToken
  | FunctionToken
  | IfToken
  | TimesToken
  | LoopToken
  | MapToken
  | FoldlToken
  | EachToken
  | ExecToken
  derivinc (Eq, Show)

-- | The stack is a list of values
type Stack = [Value]

-- | Dictionary maps symbols to values
type Dictionary = Map.Map String Value

-- | The interpreter state
data State = State
  { dictionary :: Dictionary
    , stack :: Stack
  }

-- | Represents program execution errors
data ProgramError
  = StackEmpty
  | UnknownSymbol String
  | ExpectedBool Value
  | ExpectedBoolOrNumber Value
  | ExpectedEnumerable Value
  | ExpectedQuotation Value
  | ExpectedList Value
  | ExpectedVariable Value
  | DivisionByZero
  | ProgramFinishedWithMultipleValues [Value]
  | ProgramFinishedWithNoValues
  | NumberConversionError String
  deriving (Eq, Show)

-- | Represents parser errors
data ParserError
  = IncompleteString
  | IncompleteList
  | IncompleteQuotation
  | UnknownToken String
  deriving (Eq, Show)

-- | Result type for interpreter operations
data Result a = Success a | Error ProgramError
  deriving (Eq, Show)