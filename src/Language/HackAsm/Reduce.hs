module Language.HackAsm.Reduce (toBinary) where

import Language.HackAsm.Syntax
import Text.IntFormats (showInt, IntFormat(Hexadecimal))
import Control.Monad.StatusMessage

-- |Converts a list of instructions to machine code
toBinary :: [Instruction] -> StatusMessage [Int]
toBinary = mapM reduce

-- |Converts a single instruction to machine code. Requires that all symbols be converted.
--  Checks that addresses are within bounds.
reduce :: Instruction -> StatusMessage Int
reduce (AStatement (Literal n)) = if n < 0x8000 && n >= -0x4000
                                  then return n
                                  else fail ("Error: Address " ++ showInt True Hexadecimal n ++ " is greater than maximum address 0x7fff\n")
reduce (AStatement (Relative _)) = fail "Error: All addresses supplied to Reduce must already be literal\n"
reduce (CStatement r x j) = (\n -> 0xe000 + reduceRegisters r + n + reduceJump j) <$> reduceExpression x

-- |Get the bits that determine the destination
reduceRegisters :: RegisterList -> Int
reduceRegisters (StoreADM a d m) = (if a then 0x0020 else 0) + (if d then 0x0010 else 0) + (if m then 0x0008 else 0)

-- |Get the bits that determine the jump
reduceJump :: Jump -> Int
reduceJump Continue = 0
reduceJump JGT = 1
reduceJump JEQ = 2
reduceJump JGE = 3
reduceJump JLT = 4
reduceJump JNE = 5
reduceJump JLE = 6
reduceJump JMP = 7

-- |Get the bits that determine the computation. Emits a warning if a weird construct is used.
reduceExpression :: Operation -> StatusMessage Int
reduceExpression (Constant Const0)    = return c0
reduceExpression (Constant Const1)    = return c1
reduceExpression (Constant RegisterA) = return cA
reduceExpression (Constant RegisterD) = return cD
reduceExpression (Constant RegisterM) = return cM
reduceExpression (Not Const0)         = return cn1
reduceExpression (Not Const1)         = return cn2
reduceExpression (Not RegisterA)      = return flipA
reduceExpression (Not RegisterD)      = return flipD
reduceExpression (Not RegisterM)      = return flipM
reduceExpression (Negate Const0)      = warn "Warning: -0: Why not 0?\n" $ return c0
reduceExpression (Negate Const1)      = return cn1
reduceExpression (Negate RegisterA)   = return nA
reduceExpression (Negate RegisterD)   = return nD
reduceExpression (Negate RegisterM)   = return nM

reduceExpression (Add Const0 Const0)        = warn "Warning: 0+0: Why not 0?\n" $ return c0
reduceExpression (Add Const0 Const1)        = warn "Warning: 0+1: Why not 1?\n" $ return c1
reduceExpression (Add Const1 Const0)        = warn "Warning: 1+0: Why not 1?\n" $ return c1
reduceExpression (Add Const1 Const1)        = fail "Error: 1+1 is not a supported instruction\n"
reduceExpression (Add Const0 RegisterA)     = warn "Warning: 0+A: Why not A?\n" $ return cA
reduceExpression (Add RegisterA Const0)     = warn "Warning: A+0: Why not A?\n" $ return cA
reduceExpression (Add Const0 RegisterD)     = warn "Warning: 0+D: Why not D?\n" $ return cD
reduceExpression (Add RegisterD Const0)     = warn "Warning: D+0: Why not D?\n" $ return cD
reduceExpression (Add Const0 RegisterM)     = warn "Warning: 0+M: Why not M?\n" $ return cM
reduceExpression (Add RegisterM Const0)     = warn "Warning: M+0: Why not M?\n" $ return cM
reduceExpression (Add Const1 RegisterA)     = return addA1
reduceExpression (Add RegisterA Const1)     = return addA1
reduceExpression (Add Const1 RegisterD)     = return addD1
reduceExpression (Add RegisterD Const1)     = return addD1
reduceExpression (Add Const1 RegisterM)     = return addM1
reduceExpression (Add RegisterM Const1)     = return addM1
reduceExpression (Add RegisterA RegisterA)  = fail "Error: A+A is not a supported instruction\n"
reduceExpression (Add RegisterD RegisterD)  = fail "Error: D+D is not a supported instruction\n"
reduceExpression (Add RegisterM RegisterM)  = fail "Error: M+M is not a supported instruction\n"
reduceExpression (Add RegisterA RegisterM)  = fail "Error: Can't use A and M in same operation\n"
reduceExpression (Add RegisterM RegisterA)  = fail "Error: Can't use A and M in same operation\n"
reduceExpression (Add RegisterA RegisterD)  = return addAD
reduceExpression (Add RegisterD RegisterA)  = return addAD
reduceExpression (Add RegisterM RegisterD)  = return addMD
reduceExpression (Add RegisterD RegisterM)  = return addMD

reduceExpression (Subtract Const0 Const0)        = warn "Warning: 0-0: Why not 0?\n" $ return c0
reduceExpression (Subtract Const0 Const1)        = warn "Warning: 0-1: Why not -1?\n" $ return cn1
reduceExpression (Subtract Const1 Const0)        = warn "Warning: 1-0: Why not 1?\n" $ return c1
reduceExpression (Subtract Const1 Const1)        = warn "Warning: 1-1: Why not 0?\n" $ return c0
reduceExpression (Subtract Const0 RegisterA)     = warn "Warning: 0-A: Why not -A?\n" $ return nA
reduceExpression (Subtract RegisterA Const0)     = warn "Warning: A-0: Why not A?\n" $ return cA
reduceExpression (Subtract Const0 RegisterD)     = warn "Warning: 0-D: Why not -D?\n" $ return nD
reduceExpression (Subtract RegisterD Const0)     = warn "Warning: D-0: Why not D?\n" $ return cD
reduceExpression (Subtract Const0 RegisterM)     = warn "Warning: 0-M: Why not -M?\n" $ return nM
reduceExpression (Subtract RegisterM Const0)     = warn "Warning: M-0: Why not M?\n" $ return cM
reduceExpression (Subtract Const1 RegisterA)     = fail "Error: 1-A is not a supported operation\n"
reduceExpression (Subtract RegisterA Const1)     = return subA1
reduceExpression (Subtract Const1 RegisterD)     = fail "Error: 1-D is not a supported operation\n"
reduceExpression (Subtract RegisterD Const1)     = return subD1
reduceExpression (Subtract Const1 RegisterM)     = fail "Error: 1-M is not a supported operation\n"
reduceExpression (Subtract RegisterM Const1)     = return subM1
reduceExpression (Subtract RegisterA RegisterA)  = warn "Warning: A-A: Why not 0?\n" $ return c0
reduceExpression (Subtract RegisterD RegisterD)  = warn "Warning: D-D: Why not 0?\n" $ return c0
reduceExpression (Subtract RegisterM RegisterM)  = warn "Warning: M-M: Why not 0?\n" $ return c0
reduceExpression (Subtract RegisterA RegisterM)  = fail "Error: Can't use A and M in same operation\n"
reduceExpression (Subtract RegisterM RegisterA)  = fail "Error: Can't use A and M in same operation\n"
reduceExpression (Subtract RegisterA RegisterD)  = return subAD
reduceExpression (Subtract RegisterD RegisterA)  = return subDA
reduceExpression (Subtract RegisterM RegisterD)  = return subMD
reduceExpression (Subtract RegisterD RegisterM)  = return subDM

reduceExpression (And Const0 Const0)        = warn "Warning: 0&0: Why not 0?\n" $ return c0
reduceExpression (And Const0 Const1)        = warn "Warning: 0&1: Why not 0?\n" $ return c0
reduceExpression (And Const1 Const0)        = warn "Warning: 1&0: Why not 0?\n" $ return c0
reduceExpression (And Const1 Const1)        = warn "Warning: 1&1: Why not 1?\n" $ return c1
reduceExpression (And Const0 RegisterA)     = warn "Warning: 0&A: Why not 0?\n" $ return c0
reduceExpression (And RegisterA Const0)     = warn "Warning: A&0: Why not 0?\n" $ return c0
reduceExpression (And Const0 RegisterD)     = warn "Warning: 0&D: Why not 0?\n" $ return c0
reduceExpression (And RegisterD Const0)     = warn "Warning: D&0: Why not 0?\n" $ return c0
reduceExpression (And Const0 RegisterM)     = warn "Warning: 0&M: Why not 0?\n" $ return c0
reduceExpression (And RegisterM Const0)     = warn "Warning: M&0: Why not 0?\n" $ return c0
reduceExpression (And Const1 RegisterA)     = fail "Error: 1&A is not a supported operation\n"
reduceExpression (And RegisterA Const1)     = fail "Error: A&1 is not a supported operation\n"
reduceExpression (And Const1 RegisterD)     = fail "Error: 1&D is not a supported operation\n"
reduceExpression (And RegisterD Const1)     = fail "Error: D&1 is not a supported operation\n"
reduceExpression (And Const1 RegisterM)     = fail "Error: 1&M is not a supported operation\n"
reduceExpression (And RegisterM Const1)     = fail "Error: M&1 is not a supported operation\n"
reduceExpression (And RegisterA RegisterA)  = warn "Warning: A&A: Why not A?\n" $ return cA
reduceExpression (And RegisterD RegisterD)  = warn "Warning: D&D: Why not D?\n" $ return cD
reduceExpression (And RegisterM RegisterM)  = warn "Warning: M&M: Why not M?\n" $ return cM
reduceExpression (And RegisterA RegisterM)  = fail "Error: Can't use A and M in same operation\n"
reduceExpression (And RegisterM RegisterA)  = fail "Error: Can't use A and M in same operation\n"
reduceExpression (And RegisterA RegisterD)  = return andAD
reduceExpression (And RegisterD RegisterA)  = return andAD
reduceExpression (And RegisterM RegisterD)  = return andMD
reduceExpression (And RegisterD RegisterM)  = return andMD

reduceExpression (Or Const0 Const0)        = warn "Warning: 0|0: Why not 0?\n" $ return c0
reduceExpression (Or Const0 Const1)        = warn "Warning: 0|1: Why not 1?\n" $ return c1
reduceExpression (Or Const1 Const0)        = warn "Warning: 1|0: Why not 1?\n" $ return c1
reduceExpression (Or Const1 Const1)        = warn "Warning: 1|1: Why not 1?\n" $ return c1
reduceExpression (Or Const0 RegisterA)     = warn "Warning: 0|A: Why not A?\n" $ return cA
reduceExpression (Or RegisterA Const0)     = warn "Warning: A|0: Why not A?\n" $ return cA
reduceExpression (Or Const0 RegisterD)     = warn "Warning: 0|D: Why not D?\n" $ return cD
reduceExpression (Or RegisterD Const0)     = warn "Warning: D|0: Why not D?\n" $ return cD
reduceExpression (Or Const0 RegisterM)     = warn "Warning: 0|M: Why not M?\n" $ return cM
reduceExpression (Or RegisterM Const0)     = warn "Warning: M|0: Why not M?\n" $ return cM
reduceExpression (Or Const1 RegisterA)     = fail "Error: 1|A is not a supported operation\n"
reduceExpression (Or RegisterA Const1)     = fail "Error: A|1 is not a supported operation\n"
reduceExpression (Or Const1 RegisterD)     = fail "Error: 1|D is not a supported operation\n"
reduceExpression (Or RegisterD Const1)     = fail "Error: D|1 is not a supported operation\n"
reduceExpression (Or Const1 RegisterM)     = fail "Error: 1|M is not a supported operation\n"
reduceExpression (Or RegisterM Const1)     = fail "Error: M|1 is not a supported operation\n"
reduceExpression (Or RegisterA RegisterA)  = warn "Warning: A|A: Why not A?\n" $ return cA
reduceExpression (Or RegisterD RegisterD)  = warn "Warning: D|D: Why not D?\n" $ return cD
reduceExpression (Or RegisterM RegisterM)  = warn "Warning: M|M: Why not M?\n" $ return cM
reduceExpression (Or RegisterA RegisterM)  = fail "Error: Can't use A and M in same operation\n"
reduceExpression (Or RegisterM RegisterA)  = fail "Error: Can't use A and M in same operation\n"
reduceExpression (Or RegisterA RegisterD)  = return orAD
reduceExpression (Or RegisterD RegisterA)  = return orAD
reduceExpression (Or RegisterM RegisterD)  = return orMD
reduceExpression (Or RegisterD RegisterM)  = return orMD

-- documented instructions
andAD :: Int
andAD = 0x00 * 0x40
-- 0000000 = D&A
addAD :: Int
addAD = 0x02 * 0x40
-- 0000010 = D+A
subAD :: Int
subAD = 0x07 * 0x40
-- 0000111 = A-D
c0    :: Int
c0    = 0x2a * 0x40
-- 0001000 0011000 0100000 0100100 0101000 (0101010) 0101100 0101111 0111000 0111011 0111101
--   1001000 1011000 1100000 1100100 1101000 1101010 1101100 1101111 1111000 1111011 1111101 = 0
cn1   :: Int
cn1   = 0x3a * 0x40
-- 0001001 0011001 0100001 0100101 0101001 0101011 0101101 0101110 0111001 (0111010) 0111100
--   1001001 1011001 1100101 1101001 1101011 1101101 1101101 1101110 1111001 1111010 1111100 = -1
cD    :: Int
cD    = 0x0c * 0x40
-- 0001010 (0001100) 0011011 0011101 1001010 1001100 1011011 1011101 = D
flipD :: Int
flipD = 0x0d * 0x40
-- 0001011 (0001101) 0011010 0011100 1001011 1001101 1011010 1011100 = !D
subD1 :: Int
subD1 = 0x0e * 0x40
-- (0001110) 1001110 = D-1
nD    :: Int
nD    = 0x0f * 0x40
-- (0001111) 1001111 = -D
subDA :: Int
subDA = 0x13 * 0x40
-- 0010011 = D-A
orAD  :: Int
orAD  = 0x15 * 0x40
-- 0010101 = A|D
addD1 :: Int
addD1 = 0x1f * 0x40
-- (0011111) 1011111 = D+1
cA    :: Int
cA    = 0x30 * 0x40
-- 0100010 0100111 (0110000) 0110101 = A
flipA :: Int
flipA = 0x31 * 0x40
-- 0100011 0100110 (0110001) 0110100 = !A
subA1 :: Int
subA1 = 0x32 * 0x40
-- 0110010 = A-1
nA    :: Int
nA    = 0x33 * 0x40
-- 0110011 = -A
addA1 :: Int
addA1 = 0x37 * 0x40
-- 0110111 = A+1
cn2   :: Int
cn2   = 0x3e * 0x40
-- (0111110) 1111110 = -2
c1    :: Int
c1    = 0x3f * 0x40
-- (0111111) 1111111 = 1
andMD :: Int
andMD = 0x40 * 0x40
-- 1000000 = D&M
addMD :: Int
addMD = 0x42 * 0x40
-- 1000010 = D+M
subMD :: Int
subMD = 0x47 * 0x40
-- 1000111 = M-D
subDM :: Int
subDM = 0x53 * 0x40
-- 1010011 = D-M
orMD  :: Int
orMD  = 0x55 * 0x40
-- 1010101 = M|D
cM    :: Int
cM    = 0x70 * 0x40
-- 1100010 1100111 (1110000) 1110101 = M
flipM :: Int
flipM = 0x71 * 0x40
-- 1100011 1100110 (1110001) 1110100 = !M
subM1 :: Int
subM1 = 0x72 * 0x40
-- 1110010 = M-1
nM    :: Int
nM    = 0x73 * 0x40
-- 1110011 = -M
addM1 :: Int
addM1 = 0x77 * 0x40
-- 1110111 = M+1
-- undocumented instructions
-- 0000001 = !D | !A
-- 0000011 = -D-A-1
-- 0000100 = D & !A
-- 0000101 = !D | A
-- 0000110 = D-A-1
-- 0010000 = A & !D
-- 0010001 = !A | D
-- 0010010 = A-D-1
-- 0010100 = !A & !D
-- 0010110 = -A-D-2
-- 0010111 = A+D+1
-- 0011110 1011110 = -D-2
-- 0110110 = -A-2
-- 1000001 = !D | !M
-- 1000011 = -D-M-1
-- 1000100 = D & !M
-- 1000101 = !D | M
-- 1000110 = D-M-1
-- 1010000 = M & !D
-- 1010001 = !M | D
-- 1010010 = M-D-1
-- 1010100 = !M & !D
-- 1010110 = -M-D-2
-- 1010111 = M+D+1
-- 1110110 = -M-2
