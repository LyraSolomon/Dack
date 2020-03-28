module Language.HackAsm.Syntax (Statement(..), Instruction(..), Address(..), Operation(..), RegisterList(..), Jump(..), Label(..), Value(..)) where

import Data.List (intercalate)

data Statement = Instruction Instruction | LStatement Label | Comment deriving (Eq)
data Instruction = AStatement Address | CStatement RegisterList Operation Jump deriving (Eq)
data Address = Literal Int | Relative Label deriving (Eq)
data Operation = Constant Value | Not Value | Negate Value | Add Value Value | Subtract Value Value | And Value Value | Or Value Value deriving (Eq)
data RegisterList = StoreADM Bool Bool Bool deriving (Eq)
data Jump = Continue | JEQ | JNE | JGT | JLT | JGE | JLE | JMP deriving (Eq)
newtype Label = AddrLabel String deriving (Eq)
data Value = Const0 | Const1 | RegisterA | RegisterD | RegisterM deriving (Eq)

instance Show Value where
  show Const0 = "0"
  show Const1 = "1"
  show RegisterA = "A"
  show RegisterD = "D"
  show RegisterM = "M"

instance Show Label where
  show (AddrLabel s) = s

instance Show Jump where
  show Continue = ""
  show JEQ = "JEQ"
  show JNE = "JNE"
  show JGT = "JGT"
  show JLT = "JLT"
  show JGE = "JGE"
  show JLE = "JLE"
  show JMP = "JMP"

instance Show RegisterList where
  show (StoreADM a d m) = (if a then "A" else "") ++ (if m then "M" else "") ++ (if d then "D" else "")

instance Show Operation where
  show (Constant x) = show x
  show (Not x) = '!' : show x
  show (Negate x) = '-' : show x
  show (Add x y) = show x ++ '+' : show y
  show (Subtract x y) = show x ++ '-' : show y
  show (And x y) = show x ++ '&' : show y
  show (Or x y) = show x ++ '|' : show y

instance Show Address where
  show (Literal x) = show x
  show (Relative x) = show x

instance Show Instruction where
  show (CStatement r o j) = (if r' == "" then "" else r' ++ "=") ++ show o ++ (if j' == "" then "" else ';' : j') where r' = show r; j' = show j
  show (AStatement a) = '@' : show a

instance Show Statement where
  show (Instruction i) = show i
  show (LStatement l) = '(' : show l ++ ")"
  show Comment = "//"
  showList = showString . intercalate "\n" . map show
