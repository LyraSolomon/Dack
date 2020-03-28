module Language.StackVm.Syntax where
import Prelude hiding (Ord(..))
import Data.Char (toLower)
import Data.List (intercalate)

data Segment = Argument | Local | Static | Constant | This | That | Pointer | Temp deriving Show
data Command = Add | Sub | Neg | Eq | Gt | Lt | And | Or | Not | Push Segment Int | Pop Segment Int |
               Label String | Goto String | IfGoto String | Function String Int | Call String Int | Return | Comment

instance Show Command where
  show Add = "add"
  show Sub = "sub"
  show Neg = "neg"
  show Eq  = "eq"
  show Gt  = "gt"
  show Lt  = "lt"
  show And = "and"
  show Or  = "or"
  show Not = "not"
  show (Push s i) = "push " ++ map toLower (show s) ++ ' ' : show i
  show (Pop s i) = "pop " ++ map toLower (show s) ++ ' ' : show i
  show (Label s) = "label " ++ s
  show (Goto s) = "goto " ++ s
  show (IfGoto s) = "if-goto " ++ s
  show (Function s i) = "function " ++ s ++ ' ' : show i
  show (Call s i) = "call " ++ s ++ ' ' : show i
  show Return = "return"
  show Comment = "//"
  showList = showString . intercalate "\n" . map show
