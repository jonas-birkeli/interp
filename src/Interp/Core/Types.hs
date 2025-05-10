module Interp.Core.Types (
    Value(..),
    Token(..),
    ControlOp(..),
    DefOp(..),
    Program,
    Stack,
    Interrupt,
    InterpreterState(..),
    initialState,

) where

import qualified Data.Map as Map

-- | Value types in the interp language
data Value
    = IntValue Integer
    | FloatValue Double
    | BoolValue Bool
    | StringValue String
    | ListValue [Value]
    | QuotationValue [Token]
    | SymbolValue String
    deriving (Eq)

-- | Custom show instance for values
instance Show Value where
    show (IntValue i) = show i
    show (FloatValue f) = show f
    show (BoolValue b) = show b
    show (StringValue s) = show s
    show (ListValue vs) = show vs
    show (QuotationValue ts) = "{ " ++ unwords (map show ts) ++ " }"
    show (SymbolValue s) = s

-- | Token types
data Token
    = LiteralToken Value
    | OperatorToken String
    | ControlToken ControlOp
    | DefinitionToken DefOp
    deriving (Eq)

-- | Control operations
data ControlOp
    = If
    | Times
    | Loop
    | Each
    | Map
    | Foldl
    | Exec
    deriving (Eq, Show)

-- | Definition operations
data DefOp
    = Assign
    | FunDef
    deriving (Eq, Show)

-- | Custom show instance for tokens
instance Show Token where
    show (LiteralToken v) = show v
    show (OperatorToken op) = op
    show (ControlToken ctrl) = case ctrl of
        If -> "if"
        Times -> "times"
        Loop -> "loop"
        Each -> "each"
        Map -> "map"
        Foldl -> "foldl"
        Exec -> "exec"
    show (DefinitionToken def) = case def of
        Assign -> ":="
        FunDef -> "fun"

-- | Program is a list of tokens
type Program = [Token]

-- | Stack is a list of values
type Stack = [Value]

-- | Dictionary maps symbols to values
type Dictionary = Map.Map String Value

-- | Interrupt for IO operations
data Interrupt
    = PrintInterrupt String
    | ReadInterrupt (String -> InterpreterState)

-- | Interpreter state
data InterpreterState = InterpreterState
    { dict :: Dictionary
    , stack :: Stack
    , program :: Program
    , interrupt :: Maybe Interrupt
    }

-- | Create initial state
initialState :: InterpreterState
initialState = InterpreterState
    { dict = Map.empty
    , stack = []
    , program = []
    , interrupt = Nothing
    }