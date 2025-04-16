module Types where

-- | The stack is a list of values
type Stack = [Value]

-- | Dictionary maps symbols to values
type Dictionary = Map.Map String Value

-- | The interpreter state
data State = State
  { dictionary :: Dictionary
  , stack :: Stack
  }