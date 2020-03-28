module Language.HackAsm (fromString, toMachineLang) where

import Language.HackAsm.Syntax
import Language.HackAsm.Parse
import Language.HackAsm.Process
import Language.HackAsm.Reduce
import Control.Monad.StatusMessage
import Control.Monad ((<=<))

-- |Parses an assembly source file
fromString :: String -> StatusMessage [Statement]
fromString = fromEither . parseFile

-- |Assembles a program
toMachineLang :: [Statement] -> StatusMessage [Int]
toMachineLang = toBinary <=< resolveLabels
