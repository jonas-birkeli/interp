module Interpreter.State
    (
        initialState
    ) where

import Types
import qualified Data.Map as Map

-- | Initial interpreter state
initialState :: State
initialState = State
    {
        dictionary = Map.empty,
        stack = [],
        printBuffer = []
    }