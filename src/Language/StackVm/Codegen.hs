module Language.StackVm.Codegen (codegen, boot) where

import qualified Language.StackVm.Syntax as Vm
import qualified Language.HackAsm.Syntax as Asm
import Language.StackVm.Validate (FunctionBlock, LabeledLine)
import Data.List (intercalate, replicate)

-- |Boot sequence sets @SP=256@ then calls @Sys.init@
boot :: String
boot = map (\c -> case c of ' ' -> '\n'; x -> x)
           "@261 D=A @SP AM=D D=0 M=D A=A-1 M=D A=A-1 M=D A=A-1 M=D D=A-1 @R13 M=D @$loop D=A @R13 A=M M=D @Sys.init 0;JMP ($loop) @$loop 0;JMP "

-- |Convert a program to assembly
codegen :: [FunctionBlock] -> String
codegen = map (\c -> case c of ' ' -> '\n'; x -> x) . concatMap genFunction

-- |Convert a function to assembly
genFunction :: FunctionBlock -> String
genFunction (f, _, xs) = concatMap (codegenHelper f) xs

-- TODO optimize
-- |Convert a line to assembly
codegenHelper :: String -> LabeledLine -> String
codegenHelper _ (_, Vm.Add) = "@SP AM=M-1 D=M A=A-1 M=M+D "
codegenHelper _ (_, Vm.Sub) = "@SP AM=M-1 D=M A=A-1 M=M-D "
codegenHelper _ (_, Vm.Neg) = "@SP A=M-1 M=-M "
codegenHelper _ (l, Vm.Eq) = genComparison l "JEQ"
codegenHelper _ (l, Vm.Lt) = genComparison l "JLT"
codegenHelper _ (l, Vm.Gt) = genComparison l "JGT"
codegenHelper _ (_, Vm.And) = "@SP AM=M-1 D=M A=A-1 M=M&D "
codegenHelper _ (_, Vm.Or) = "@SP AM=M-1 D=M A=A-1 M=M|D "
codegenHelper _ (_, Vm.Not) = "@SP A=M-1 M=!M "
codegenHelper _ (l, Vm.Push s i) = loadVar l s i ++ "@SP A=M M=D @SP M=M+1 "
codegenHelper _ (l, Vm.Pop s i) = "@SP AM=M-1 D=M " ++ storeVar l s i
codegenHelper f (_, Vm.Label s) = '(' : f ++ '$' : s ++ ") "
codegenHelper f (_, Vm.Goto s) = '@' : f ++ '$' : s ++ " 0;JMP "
codegenHelper f (_, Vm.IfGoto s) = "@SP AM=M-1 D=M @" ++ f ++ '$' : s ++ " D;JNE "
-- stack layout: [f] stack [g] arg0 arg1 [f] retaddr LCL ARG THIS THAT [g] lcl0 lcl1 stack
codegenHelper _ (l, Vm.Call f n) = '@' : fst l ++ '$' : show (snd l) ++ "$return D=A @SP A=M M=D " ++ -- push retaddr
                                "@LCL D=M @SP AM=M+1 M=D " ++ -- push LCL (SP one lower)
                                "@ARG D=M @SP AM=M+1 M=D " ++ -- push ARG (SP one lower)
                                "@THIS D=M @SP AM=M+1 M=D " ++ -- push THIS
                                "@THAT D=M @SP AM=M+1 M=D " ++ -- push THAT
                                "@SP DM=M+1 @LCL M=D " ++ -- fix SP, LCL=SP
                                '@' : show (n+5) ++ " D=D-A @ARG M=D " ++ -- ARG=SP-5-n
                                '@' : f ++ " 0;JMP (" ++ fst l ++ '$' : show (snd l) ++ "$return) " -- transfer control
codegenHelper _ (_, Vm.Function f 0) = '(' : f ++ ") "
codegenHelper _ (_, Vm.Function f k) = '(' : f ++ ") @SP A=M " ++ intercalate "A=A+1 " (replicate k "M=0 ") ++ "D=A @SP M=D+1 "
codegenHelper _ (_, Vm.Return) = "@LCL D=M @5 A=D-A D=M @R13 M=D " ++ -- RET = *(FRAME-5)
                            "@SP A=M-1 D=M @ARG A=M M=D " ++ -- (*ARG) = *(SP-1)
                            "D=A+1 @SP M=D " ++ -- SP=ARG+1
                            "@LCL AM=M-1 D=M @THAT M=D " ++ -- THAT=*(FRAME-1), where FRAME is the original value of LCL
                            "@LCL AM=M-1 D=M @THIS M=D " ++ -- THIS=*(FRAME-2)
                            "@LCL AM=M-1 D=M @ARG M=D " ++ -- ARG=*(FRAME-3)
                            "@LCL A=M-1 D=M @LCL M=D " ++ -- LCL = *(FRAME-4)
                            "@R13 A=M;JMP " -- goto RET
codegenHelper _ (_, Vm.Comment) = ""

-- |Put the specified value into the @D@ register
loadVar :: (String, Int) -> Vm.Segment -> Int -> String
loadVar _ Vm.Constant i = '@' : show i ++ " D=A "
loadVar _ Vm.Local i = '@' : show i ++ " D=A @LCL A=M+D D=M "
loadVar _ Vm.Argument i = '@' : show i ++ " D=A @ARG A=M+D D=M "
loadVar _ Vm.This i = '@' : show i ++ " D=A @THIS A=M+D D=M "
loadVar _ Vm.That i = '@' : show i ++ " D=A @THAT A=M+D D=M "
loadVar _ Vm.Temp i = '@' : show (i + 5) ++ " D=M "
loadVar l Vm.Static i = '@' : fst l ++ '$' : show i ++ " D=M "
loadVar _ Vm.Pointer 0 = "@THIS D=M "
loadVar _ Vm.Pointer 1 = "@THAT D=M "
loadVar _ Vm.Pointer _ = undefined

-- |Store the value in the @D@ register in the specified location
storeVar :: (String, Int) -> Vm.Segment -> Int -> String
storeVar _ Vm.Constant _ = undefined
storeVar _ Vm.Local i = storeSegment "LCL" i
storeVar _ Vm.Argument i = storeSegment "ARG" i
storeVar _ Vm.This i = storeSegment "THIS" i
storeVar _ Vm.That i = storeSegment "THAT" i
storeVar _ Vm.Temp i = '@' : show (i + 5) ++ " M=D "
storeVar l Vm.Static i = '@' : fst l ++ '$' : show i ++ " M=D "
storeVar _ Vm.Pointer 0 = "@THIS M=D "
storeVar _ Vm.Pointer 1 = "@THAT M=D "
storeVar _ Vm.Pointer _ = undefined

storeSegment :: String -> Int -> String
storeSegment s i = "@R13 M=D @" ++ show i ++ " D=A @" ++ s ++ " D=D+M @R14 M=D @R13 D=M @R14 A=M M=D "

-- |Convert a comparison to assembly
genComparison :: (String, Int) -> String -> String
genComparison l cmp = "@SP AM=M-1 D=M A=A-1 D=M-D @" ++ fst l ++ '$' : show (snd l) ++ "$true D;" ++ cmp ++
                      " @SP A=M-1 M=0 @" ++ fst l ++ '$' : show (snd l) ++ "$false 0;JMP " ++
                      '(' : fst l ++ '$' : show (snd l) ++ "$true) @SP A=M-1 M=-1 (" ++ fst l ++ '$' : show (snd l) ++ "$false) "
