module Language.HackAsm.Eval (MachineState(..), Memory, Program, initMachine, step) where

import Data.Array
import Data.Bits
import Language.HackAsm.Syntax

-- |The memory of the virtual mechine, which is an int16[0xffff]
type Memory = Array Int Int
-- |The instructions that make up the program
type Program = Array Int Instruction
-- |The entire virtual machine, including PC, A, and D registers
data MachineState = MachineState Memory Program Int Int Int

-- |Given a program, boot a virtual machine that runs it
initMachine :: [Instruction] -> MachineState
initMachine xs = let program = array (0, length xs - 1) (zip [0..] xs)
                     memory = array (0, 0xffff) []
                 in MachineState memory program 0 undefined undefined

-- |Run the next instruction of the virtual CPU
step (MachineState m f pc a d) = let (m', pc', a', d') = step' (f ! pc) in MachineState m' f pc' a' d'
  where step' (AStatement (Literal x)) = (m, pc+1, x, d)
        step' (CStatement (StoreADM _a _d _m) op jmp) = let val = runOp op
                                                            shouldJump = runJump jmp val
                                                        in (if _m then m // [(a, val)] else m,
                                                            if shouldJump then if _a then val else a else pc+1, -- A gets set before the jump target is read
                                                            if _a then val else a,
                                                            if _d then val else d)
        -- Get the result of a computation
        runOp (Constant x) = load x
        runOp (Negate x) = (0x10000 - load x) .&. 0xffff
        runOp (Not x) = complement (load x) .&. 0xffff
        runOp (Add x y) = (load x + load y) .&. 0xffff
        runOp (Subtract x y) = (load x - load y) .&. 0xffff
        runOp (And x y) = load x .&. load y
        runOp (Or x y) = load x .|. load y
        load Const0 = 0
        load Const1 = 1
        load RegisterA = a
        load RegisterD = d
        load RegisterM = m ! a
        -- Determines whether jump condition is met, accounting for sign bit
        runJump Continue _ = False
        runJump JMP _ = True
        runJump JEQ x = x == 0
        runJump JNE x = x /= 0
        runJump JGT x = x .&. 0x8000 == 0 && x /= 0
        runJump JGE x = x .&. 0x8000 == 0
        runJump JLT x = x .&. 0x8000 /= 0
        runJump JLE x = x .&. 0x8000 /= 0 || x == 0
