module Interp.Core.State(
   initialState,
   setProgram,
   setStack,
   setDict,
   setInterrupt,
   clearInterrupt,
   clearStack,
   clearProgram,
   resetState
) where

import Interp.Core.Types
import qualified Data.Map as Map

-- | Create initial state
--
-- >>> dict initialState
-- fromList []
--
-- >>> stack initialState
-- []
--
-- >>> program initialState
-- []
--
-- >>> interrupt initialState
-- Nothing
--
initialState :: InterpreterState
initialState = InterpreterState
    { dict = Map.empty
    , stack = []
    , program = []
    , interrupt = Nothing
    }

-- | Set the program in state
--
-- >>> program (setProgram [LiteralToken (IntValue 42)] initialState)
-- [LiteralToken (IntValue 42)]
--
setProgram :: Program -> InterpreterState -> InterpreterState
setProgram prog state = state { program = prog }

-- | Set the stack in state
--
-- >>> stack (setStack [IntValue 42, IntValue 10] initialState)
-- [IntValue 42,IntValue 10]
--
setStack :: Stack -> InterpreterState -> InterpreterState
setStack st state = state { stack = st }

-- | Set the dictionary in state
--
-- >>> dict (setDict (Map.singleton "x" (IntValue 42)) initialState)
-- fromList [("x",IntValue 42)]
--
setDict :: Dictionary -> InterpreterState -> InterpreterState
setDict d state = state { dict = d }

-- | Set an interrupt in state
--
-- >>> case setInterrupt (PrintInterrupt "hello") initialState of
-- ...   InterpreterState _ _ _ (Just (PrintInterrupt msg)) -> msg
-- ...   _ -> ""
-- "hello"
--
setInterrupt :: Interrupt -> InterpreterState -> InterpreterState
setInterrupt i state = state { interrupt = Just i }

-- | Clear the interrupt
--
-- >>> interrupt (clearInterrupt (setInterrupt (PrintInterrupt "test") initialState))
-- Nothing
--
clearInterrupt :: InterpreterState -> InterpreterState
clearInterrupt state = state { interrupt = Nothing }

-- | Clear the stack
--
-- >>> stack (clearStack (setStack [IntValue 1, IntValue 2] initialState))
-- []
--
clearStack :: InterpreterState -> InterpreterState
clearStack state = state { stack = [] }

-- | Clear the program
--
-- >>> program (clearProgram (setProgram [LiteralToken (IntValue 42)] initialState))
-- []
--
clearProgram :: InterpreterState -> InterpreterState
clearProgram state = state { program = [] }

-- | Reset state to initial, keeping dictionary
--
-- >>> let state = initialState { dict = Map.singleton "x" (IntValue 42), stack = [IntValue 1], program = [OperatorToken "+"] }
-- >>> let reset = resetState state
-- >>> (dict reset, stack reset, program reset)
-- (fromList [("x",IntValue 42)],[],[])
--
resetState :: InterpreterState -> InterpreterState
resetState state = initialState { dict = dict state }