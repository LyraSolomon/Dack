module Language.StackVm.Eval (MachineState(..), Memory, Program, initMachine, step) where

import Data.Array
import qualified Data.Map.Strict as Map
import Data.Bits
import Language.StackVm.Syntax
import Language.StackVm.Validate (FunctionBlock, LabeledLine)
import Data.List (groupBy)

-- |The memory of the virtual mechine, which is an int16[0xffff]
type Memory = Array Int Int
-- |The instructions that make up the program, and the location of labels
type Program = (Array Int Command, Map.Map String Int)
-- |The entire virtual machine
data MachineState = MachineState Memory Program Int

-- |Given a program, boot a virtual machine that runs it
initMachine :: [FunctionBlock] -> MachineState
initMachine xs = let -- allocate static segments, mark labels with functions, then concatenate functions
                     xs' = zip [0..] . concatMap (\(s, _, f) -> map (\(_, x) -> case x of Label l  -> Label  (s ++ '$' : l)
                                                                                          Goto l   -> Goto   (s ++ '$' : l)
                                                                                          IfGoto l -> IfGoto (s ++ '$' : l)
                                                                                          _ -> x) f) . alignStatic $ xs
                     labels = Map.fromList (getLabels xs') -- map from labelName -> instruction address
                     program = array (0, length xs' - 1) xs'
                     firstLine = labels Map.! "Sys.init"
                     memory = array (0, 0xffff) [(0, 261)]
                 in MachineState memory (program, labels) firstLine
  where getLabels ((i, Label s) : xs) = (s, i) : getLabels xs
        getLabels ((i, Function s _) : xs) = (s, i) : getLabels xs
        getLabels (_:xs) = getLabels xs
        getLabels [] = []

-- |Run the next instruction of the virtual machine
step :: MachineState -> MachineState
step (MachineState m p i) = let (m', i') = step' (fst p ! i) in MachineState m' p i'
  where step' Add = let sp = m!0 in (m // [(0, sp-1), (sp-2, ((m!(sp-2)) + (m!(sp-1))) .&. 0xffff)], i+1)
        step' Sub = let sp = m!0 in (m // [(0, sp-1), (sp-2, ((m!(sp-2)) - (m!(sp-1))) .&. 0xffff)], i+1)
        step' And = let sp = m!0 in (m // [(0, sp-1), (sp-2, (m!(sp-2)) .&. (m!(sp-1)))], i+1)
        step' Or  = let sp = m!0 in (m // [(0, sp-1), (sp-2, (m!(sp-2)) .|. (m!(sp-1)))], i+1)
        step' Eq  = let sp = m!0 in (m // [(0, sp-1), (sp-2, if (m!(sp-2)) == (m!(sp-1)) then 0xffff else 0)], i+1)
        step' Gt  = let sp = m!0 in (m // [(0, sp-1), (sp-2, if testLt (m!(sp-2)) (m!(sp-1)) then 0xffff else 0)], i+1)
        step' Lt  = let sp = m!0 in (m // [(0, sp-1), (sp-2, if testLt (m!(sp-1)) (m!(sp-2)) then 0xffff else 0)], i+1)
        step' Neg = let sp = m!0 in (m // [(sp-1, 0x10000 - (m!(sp-1)))], i+1)
        step' Not = let sp = m!0 in (m // [(sp-1, complement (m!(sp-1)) .&. 0xffff)], i+1)
        step' (Push Constant n) = let sp = m!0 in (m // [(0, sp+1), (sp, n .&. 0xffff)], i+1)
        step' (Push s n) = let sp = m!0 in (m // [(0, sp+1), (sp, m ! getAddr s n)], i+1)
        step' (Pop Constant _) = undefined
        step' (Pop  s n) = let sp = m!0 in (m // [(0, sp-1), (getAddr s n, m!(sp-1))], i+1)
        step' (Label _) = (m, i+1)
        step' (Goto l) = (m, snd p Map.! l)
        step' (IfGoto l) = let jmp = m!((m!0)-1) in (m // [(0, (m!0)-1)], if jmp /= 0 then snd p Map.! l else i+1)
        step' (Function _ n) = let sp = m!0 in (m // ((0, sp+n) : [(sp+i, 0) | i <- [0..(n-1)]]), i+1)
        step' (Call f n) = let sp = m!0 in (m // [(sp, i+1), (sp+1, m!1), (sp+2, m!2), (sp+3, m!3), (sp+4, m!4), (2, sp-n), (1, sp+5), (0, sp+5)], snd p Map.! f)
        step' Return = let frame = m!1 in (m // [(m!2, m!((m!0)-1)), (0, (m!2)+1), (4, m!(frame-1)), (3, m!(frame-2)), (2, m!(frame-3)), (1, m!(frame-4))], m!(frame-5))
        step' Comment = (m, i+1)
        getAddr Argument i = (m!2)+i
        getAddr Local    i = (m!1)+i
        getAddr Static   i = 16+i
        getAddr Constant i = undefined
        getAddr This     i = (m!3)+i
        getAddr That     i = (m!4)+i
        getAddr Pointer  0 = 3
        getAddr Pointer  1 = 4
        getAddr Pointer  _ = undefined
        getAddr Temp     i = 5+i
        testLt x y = (x - y) .&. 0x8000 == 0 && x /= y -- Determines whether condition is met, accounting for sign bit

-- |Allocates space for static variables of each file. Adds an offset to the static indices so each file has its own unique set.
alignStatic :: [FunctionBlock] -> [FunctionBlock]
alignStatic fs = let -- Number of statics used in each file
                     ns = map (\xs@((f,_):_) -> (f, (+1) . maximum . map snd $ xs)) . groupBy (\(a, _) (b, _) -> a == b) . concatMap getStatics $ fs
                     -- Index allocated for first static in the file
                     offsets = Map.fromList . snd . foldl (\(i, xs) (f, n) -> (i + n, (f, i) : xs)) (0, []) $ ns
                 in map (replaceStatics offsets) fs
  where -- Extract list of statics in the form [(file, index)]
        getStatics :: FunctionBlock -> [(String, Int)]
        getStatics (_, _, xs) = getStatics' xs
        getStatics' :: [LabeledLine] -> [(String, Int)]
        getStatics' (((f, _), Push Static n) : xs) = (f, n) : getStatics' xs
        getStatics' (((f, _), Pop  Static n) : xs) = (f, n) : getStatics' xs
        getStatics' (_:xs) = getStatics' xs
        getStatics' [] = []
        -- Given a map of file -> offset, replace the indices of statics in a function
        replaceStatics o (f, n, xs) = (f, n, map (replaceStatics' o) xs)
        replaceStatics' o ((f, n), Push Static i) = ((f, n), Push Static (o Map.! f + i))
        replaceStatics' o ((f, n), Pop  Static i) = ((f, n), Pop  Static (o Map.! f + i))
        replaceStatics' _ l = l
