module Language.StackVm (fromString, toAsm) where

import Language.StackVm.Syntax
import Language.StackVm.Parse
import Language.StackVm.Codegen
import Language.StackVm.Validate
import Control.Monad.StatusMessage
import Control.Monad ((<=<))
import Prelude hiding (Ord(..))

-- |Parses a VM source file
fromString :: String -> StatusMessage [Command]
fromString = fromEither . parseFile

-- |Compiles a program, which may consist of several files, into assembly
toAsm :: [(String, [Command])] -> StatusMessage String
toAsm = fmap ((boot++) . codegen) . validate
