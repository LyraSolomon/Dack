{-# LANGUAGE TupleSections #-}
module Language.HackAsm.Process (resolveLabels) where

import Language.HackAsm.Syntax
import Control.Monad.Writer.Lazy
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Control.Monad.StatusMessage

-- |Attach instruction numbers to a list of statements, dropping comments.
--  Does not increment when passing a label.
instructionNumbers :: [Statement] -> [(Int, Statement)]
instructionNumbers = instructionNumbers' 0
  where instructionNumbers' n (Comment : xs) = instructionNumbers' n xs
        instructionNumbers' n (s@(LStatement _) : xs) = (n, s) : instructionNumbers' n xs
        instructionNumbers' n (s : xs) = (n, s) : instructionNumbers' (n+1) xs
        instructionNumbers' _ [] = []

-- |Filter the instructions only by dropping comments and labels
instructions :: [Statement] -> [Instruction]
instructions [] = []
instructions (Comment:xs) = instructions xs
instructions (LStatement _ : xs) = instructions xs
instructions (Instruction x : xs) = x : instructions xs

-- |Generate a list of the instruction number of each label. Warn if a label is not
--  followed by a non-label statement
userLabels :: [(Int, Statement)] -> Writer String [(String, Int)]
userLabels ((n, LStatement (AddrLabel s)) : x@(_, LStatement _) : xs) =
    tell ("Warning: label \"" ++ s ++ "\" is immediately followed by another label. Suggestion: consider combining them.\n") >>
    ((s, n):) <$> userLabels (x:xs)
userLabels [(n, LStatement (AddrLabel s))] =
    tell ("Warning: label \"" ++ s ++ "\" has no instruction following it. Suggestion: follow it by a statement that jumps to itself.\n") >>
    return [(s, n)]
userLabels ((n, LStatement (AddrLabel s)) : xs) = ((s, n):) <$> userLabels xs
userLabels (_:xs) = userLabels xs
userLabels [] = return []

-- |Generate a list of potential variable addresses. Convert each unique label used into an
--  address beginning at 16.
userVariables :: [Instruction] -> Map.Map String Int
userVariables = userVariables' Map.empty 16
  where userVariables' m _ [] = m
        userVariables' m n (AStatement (Relative (AddrLabel s)) : xs) = if s `Map.member` m
                                                                        then userVariables' m n xs
                                                                        else userVariables' (Map.insert s n m) (n+1) xs
        userVariables' m n (_:xs) = userVariables' m n xs

-- |Generate a map of all labels to their definition, based on a list of labels. Emits error if
--  a label is redefined. Begins with a list of predefined symbols.
allLabels :: [(String, Int)] -> Either String (Map.Map String (Int, Bool))
allLabels [] = Right predefined
allLabels ((s,n):xs) =
  let m = allLabels xs
  in if either (const False) (s `Map.member`) m
     then Left ("Error: redefinition of " ++ (if s `Map.member` predefined then "builtin" else "user defined") ++ " label \"" ++ s ++ "\"\n")
     else Map.insert s (n, False) <$> m


-- |Replace all instances of the labels in the map. Warn if a label is never used.
replaceLabels :: Map.Map String (Int, Bool) -> [Instruction] -> Writer String [Instruction]
replaceLabels m (x@(AStatement (Relative (AddrLabel s))) : xs) = if s `Map.member` m
                                                                 then let m' = Map.update (\(n, _) -> Just (n, True)) s m -- Mark the label as used
                                                                      in ((AStatement . Literal . fst $ m' ! s):) <$> replaceLabels m' xs
                                                                 else (x:) <$> replaceLabels m xs
replaceLabels m (x:xs) = (x:) <$> replaceLabels m xs
replaceLabels m [] = let unused = Map.foldlWithKey (\b k a -> if snd a then b else "\""++k++"\" "++b) [] m
                     in if unused == "" then writer ([], "") else writer ([], "Warning: labels "++unused++"are never used. Suggestion: consider removing them.\n")

-- |Fully remove all labels from a program
resolveLabels :: [Statement] -> StatusMessage [Instruction]
resolveLabels syntaxTree = do
  userLabels' <- fromWriter . userLabels . instructionNumbers $ syntaxTree
  allLabels' <- fromEither $ allLabels userLabels'
  withoutCodeLabels <- fromWriter . replaceLabels allLabels' $ instructions syntaxTree
  fromWriter $ replaceLabels ((, False) <$> userVariables withoutCodeLabels) withoutCodeLabels

predefined :: Map.Map String (Int, Bool)
predefined = Map.fromList [("SP", (0, True)),
                           ("LCL", (1, True)),
                           ("ARG", (2, True)),
                           ("THIS", (3, True)),
                           ("THAT", (4, True)),
                           ("R0", (0, True)),
                           ("R1", (1, True)),
                           ("R2", (2, True)),
                           ("R3", (3, True)),
                           ("R4", (4, True)),
                           ("R5", (5, True)),
                           ("R6", (6, True)),
                           ("R7", (7, True)),
                           ("R8", (8, True)),
                           ("R9", (9, True)),
                           ("R10", (10, True)),
                           ("R11", (11, True)),
                           ("R12", (12, True)),
                           ("R13", (13, True)),
                           ("R14", (14, True)),
                           ("R15", (15, True)),
                           ("SCREEN", (0x4000, True)),
                           ("KBD", (0x6000, True))]
