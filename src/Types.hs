module Types 
	(
		State(..),
		ProgramError(..),
		ParseError(..),
		Token(..),
		Value(..),
		Dictionary,
		Stack,
    ) where

import qualified Data.Map as Map

-- | Core data types for values in the language
data Value
	= IntValue Integer
	| FloatValue Double
	| BoolValue Bool
	| StringValue String
	| ListValue [Value]
	| QuotationValue [Token]
	| SymbolValue String
	deriving (Eq)

-- | Custom show instance for Value
instance Show Value where
	show (IntValue i) = show i
	show (FloatValue f) = show f
	show (BoolValue b) = show b
	show (StringValue s) = "\"" ++ s ++ "\""
	show (ListValue lv) = show lv
	show (QuotationValue qv) = "{ " ++ unwords (map showToken qv) ++ " }"
	show (SymbolValue sv) = sv

-- | Helper function to show tokens in a nice way
showToken :: Token -> String

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
	deriving (Eq, Show)

-- | The stack is a list of values
type Stack = [Value]

-- | Dictionary maps symbols to values
type Dictionary = Map.Map String Value

-- | The interpreter state
data State = State
	{ 
		dictionary :: Dictionary,  -- Maps variable names to values
		stack :: Stack,            -- The operand stack
		printBuffer :: [String]    -- Stores messages to print
	}

-- | Represents program execution errors
data ProgramError
	= StackEmpty                            	-- Stack is empty when trying to pop
	| UnknownSymbol String                  	-- Symbol not found in dictionary
	| ExpectedBool Value                    	-- Expected a boolean value
	| ExpectedBoolOrNumber Value           	 	-- Expected a boolean or numeric value
	| ExpectedEnumerable Value               	-- Expected a list, string, or quotation 
	| ExpectedQuotation Value               	-- Expected a quotation
	| ExpectedList Value                     	-- Expected a list
	| ExpectedVariable Value                    -- Expected a variable name (symbol)
	| DivisionByZero                            -- Division by zero
	| ProgramFinishedWithMultipleValues [Value] -- Program ended with multiple values on stack
	| ProgramFinishedWithNoValues          		-- Program ended with empty stack
	| NumberConversionError String         	 	-- String could not be converted to number
	deriving (Eq, Show)

-- | Represents parser errors
data ParseError
  	= IncompleteString                      	-- String literal missing closing quote
  	| IncompleteList                        	-- List missing closing bracket
  	| IncompleteQuotation                   	-- Quotation missing closing brace
  	| UnknownToken String                   	-- Could not parse token
  	| UnbalancedBraces                      	-- Unbalanced braces in program
  	| UnbalancedBrackets                    	-- Unbalanced brackets in program
  	deriving (Eq, Show)