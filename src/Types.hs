module Types where

import qualified Data.Map as Map

-- | Core data types for values in the language
data Value
  = IntValue Integer
  deriving (Eq)

-- | The stack is a list of values
type Stack = [Value]

-- | Dictionary maps symbols to values
type Dictionary = Map.Map String Value

-- | The interpreter state
data State = State
  { dictionary :: Dictionary
  , stack :: Stack
  }