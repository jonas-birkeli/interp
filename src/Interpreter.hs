{-# LANGUAGE RankNTypes #-}
module Interpreter
    ( executeProgram
    , initialState
    , executeTokenStream
    ) where

import Types (ProgramError(..), State(..), Token(..), Value(..))
import qualified Data.Map as Map
import Text.Read (readMaybe)
import Control.Monad ((>=>), foldM)





