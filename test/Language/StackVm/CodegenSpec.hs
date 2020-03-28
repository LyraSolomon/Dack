module Language.StackVm.CodegenSpec (spec) where

import Test.Hspec
import Test.QuickCheck (property)
import Control.Monad.StatusMessage
import Data.Array ((!), (//))
import Language.HackAsm.Eval
import Language.StackVm.Codegen
import qualified Language.HackAsm as Asm
import qualified Language.StackVm as Vm
import Data.Bits

-- |Unit tests for the internal module "Language.StackVm.Codegen"
spec :: Spec
spec = do
  it "Add" $ property $
    \(sp, a, b) -> let sp' = (sp .&. 0x3fff) + 256; a' = a .&. 0xffff; b' = b .&. 0xffff
                   in testAsm [(0, sp'-1), (sp'-2, (a+b) .&. 0xffff)] [(0, sp'), (sp'-2, a'), (sp'-1, b')] 10 "Add\nlabel hlt\ngoto hlt"
  it "Sub" $ property $
    \(sp, a, b) -> let sp' = (sp .&. 0x3fff) + 256; a' = a .&. 0xffff; b' = b .&. 0xffff
                   in testAsm [(0, sp'-1), (sp'-2, (a-b) .&. 0xffff)] [(0, sp'), (sp'-2, a'), (sp'-1, b')] 10 "Sub\nlabel hlt\ngoto hlt"
  it "Neg" $ property $
    \(sp, a) -> let sp' = (sp .&. 0x3fff) + 256; a' = a .&. 0xffff
                in testAsm [(0, sp'), (sp'-1, (-a) .&. 0xffff)] [(0, sp'), (sp'-1, a')] 10 "Neg\nlabel hlt\ngoto hlt"
  it "Eq"  $ property $
    \(sp, a, b) -> let sp' = (sp .&. 0x3fff) + 256; a' = a .&. 0xffff; b' = b .&. 0xffff
                   in testAsm [(0, sp'-1), (sp'-2, if a' == b' then 0xffff else 0)] [(0, sp'), (sp'-2, a'), (sp'-1, b')] 20 "Eq\nlabel hlt\ngoto hlt"
  it "Lt"  $ property $
    \(sp, a, b) -> let sp' = (sp .&. 0x3fff) + 256; a' = a .&. 0xffff; b' = b .&. 0xffff
                   in testAsm [(0, sp'-1), (sp'-2, if fixSign a' < fixSign b' then 0xffff else 0)] [(0, sp'), (sp'-2, a'), (sp'-1, b')] 20 "Lt\nlabel hlt\ngoto hlt"
  it "Gt"  $ property $
    \(sp, a, b) -> let sp' = (sp .&. 0x3fff) + 256; a' = a .&. 0xffff; b' = b .&. 0xffff
                   in testAsm [(0, sp'-1), (sp'-2, if fixSign a' > fixSign b' then 0xffff else 0)] [(0, sp'), (sp'-2, a'), (sp'-1, b')] 20 "Gt\nlabel hlt\ngoto hlt"
  it "And" $ property $
    \(sp, a, b) -> let sp' = (sp .&. 0x3fff) + 256; a' = a .&. 0xffff; b' = b .&. 0xffff
                   in testAsm [(0, sp'-1), (sp'-2, (a .&. b) .&. 0xffff)] [(0, sp'), (sp'-2, a'), (sp'-1, b')] 10 "And\nlabel hlt\ngoto hlt"
  it "Or"  $ property $
    \(sp, a, b) -> let sp' = (sp .&. 0x3fff) + 256; a' = a .&. 0xffff; b' = b .&. 0xffff
                   in testAsm [(0, sp'-1), (sp'-2, (a .|. b) .&. 0xffff)] [(0, sp'), (sp'-2, a'), (sp'-1, b')] 10  "Or\nlabel hlt\ngoto hlt"
  it "Not" $ property $
    \(sp, a) -> let sp' = (sp .&. 0x3fff) + 256; a' = a .&. 0xffff
                in testAsm [(0, sp'), (sp'-1, complement a .&. 0xffff)] [(0, sp'), (sp'-1, a')] 10 "Not\nlabel hlt\ngoto hlt"
  describe "Push" $ do
    it "Constant" $ property $
      \(sp, a) -> let sp' = (sp .&. 0x3fff) + 256; a' = a .&. 0xffff
                  in testAsm [(0, sp'+1), (sp', a')] [(0, sp')] 20 $ "push constant " ++ show a' ++ "\nlabel hlt\ngoto hlt"
    it "Local" $ property $
      \(sp, i, a) -> let sp' = (sp .&. 0x3fff) + 256; i' = i .&. 3; a' = a .&. 0xffff
                     in testAsm [(0, sp'+1), (1, sp'-4), (sp', a'), (sp'-4+i', a')] [(0, sp'), (1, sp'-4), (sp'-4+i', a')] 20 $
                                "push local " ++ show i' ++ "\nlabel hlt\ngoto hlt"
    it "Argument" $ property $
      \(sp, i, a) -> let sp' = (sp .&. 0x3fff) + 256; i' = i .&. 3; a' = a .&. 0xffff
                     in testAsm [(0, sp'+1), (2, sp'-12), (sp', a'), (sp'-12+i', a')] [(0, sp'), (2, sp'-12), (sp'-12+i', a')] 20 $
                                "push argument " ++ show i' ++ "\nlabel hlt\ngoto hlt"
    it "This" $ property $
      \(p, i, a) -> let p' = (p .&. 0x3fff) + 3000; i' = i .&. 0xff; a' = a .&. 0xffff
                    in testAsm [(0, 257), (3, p'), (256, a'), (p'+i', a')] [(0, 256), (p'+i', a')] 30 $
                               "push constant " ++ show p' ++ "\npop pointer 0\npush this " ++ show i' ++ "\nlabel hlt\ngoto hlt"
    it "That" $ property $
      \(p, i, a) -> let p' = (p .&. 0x3fff) + 4000; i' = i .&. 0xff; a' = a .&. 0xffff
                    in testAsm [(0, 257), (4, p'), (256, a'), (p'+i', a')] [(0, 256), (p'+i', a')] 30 $
                               "push constant " ++ show p' ++ "\npop pointer 1\npush that " ++ show i' ++ "\nlabel hlt\ngoto hlt"
  describe "Pop" $ do
    it "Local" $ property $
      \(sp, i, a) -> let sp' = (sp .&. 0x3fff) + 256; i' = i .&. 3; a' = a .&. 0xffff
                     in testAsm [(0, sp'), (1, sp'-4), (sp'-4+i', a')] [(0, sp'+1), (1, sp'-4), (sp', a')] 20 $ "pop local " ++ show i' ++ "\nlabel hlt\ngoto hlt"
    it "Argument" $ property $
      \(sp, i, a) -> let sp' = (sp .&. 0x3fff) + 256; i' = i .&. 3; a' = a .&. 0xffff
                     in testAsm [(0, sp'), (1, sp'-12), (sp'-12+i', a')] [(0, sp'+1), (1, sp'-12), (sp', a')] 20 $ "pop local " ++ show i' ++ "\nlabel hlt\ngoto hlt"
    it "This" $ property $
      \(p, i, a) -> let p' = (p .&. 0x3fff) + 3000; i' = i .&. 0xff; a' = a .&. 0xffff
                    in testAsm [(0, 256), (3, p'), (4, a'), (p'+i', a')] [(0, 256), (3, p'), (4, a')] 30 $ "push pointer 1\npop this " ++ show i' ++ "\nlabel hlt\ngoto htl"
    it "That" $ property $
      \(p, i, a) -> let p' = (p .&. 0x3fff) + 4000; i' = i .&. 0xff; a' = a .&. 0xffff
                    in testAsm [(0, 256), (3, a'), (4, p'), (p'+i', a')] [(0, 256), (3, a'), (4, p')] 30 $ "push pointer 0\npop that " ++ show i' ++ "\nlabel hlt\ngoto htl"
  it "Goto" $ testAsm [(0, 258), (256, 2718), (257, 1414)] [(0, 256)] 50 $ "goto label1\nlabel label2\npush constant 1414\ngoto hlt\n" ++
                                                                           "label label1\npush constant 2718\ngoto label2\nlabel hlt\ngoto hlt"
  it "IfGoto" $ testAsm [(0, 256), (5, 3141), (6, 6674)] [(0, 258), (256, 0), (257, 0xffff)] 50 $
                        "if-goto label1\npush constant 42\ngoto label2\nlabel label1\npush constant 3141\nlabel label2\npop temp 0\n" ++
                        "if-goto label3\npush constant 6674\ngoto label4\nlabel label3\npush constant 42\nlabel label4\npop temp 1\nlabel hlt\ngoto hlt"
  it "Call" $ property $
    \(sp, args, lcl, arg, this, that) -> let sp' = (sp .&. 0x3fff) + 256
                                             args' = take 10 $ map (.&. 0xffff) args
                                             lcl' = lcl .&. 0xffff
                                             arg' = arg .&. 0xffff
                                             this' = this .&. 0xffff
                                             that' = that .&. 0xffff
                                             goal = [(0, sp' + 2 * length args' + 5), (1, sp' + length args' + 5), (2, sp')] ++
                                                    zip [sp'..] args' ++ zip [(sp' + length args' + 1)..] (lcl':arg':this':that':args')
                                             mem = [(0, sp' + length args'), (1, lcl'), (2, arg'), (3, this'), (4, that')] ++ zip [sp'..] args'
                                             code = "call test " ++ show (length args') ++ "\nfunction test 0\n" ++
                                                    concatMap (\i -> "push argument " ++ show i ++ "\n") [0..(length args' - 1)] ++ "label hlt\ngoto hlt"
                                         in testAsm goal mem 200 code
  it "Function" $ property $
    \(sp, n) -> let sp' = (sp .&. 0x3fff) + 256; n' = n .&. 0x1f
                in testAsm ((0, sp'+n') : (1, sp') : zip [sp'..] (take n' $ repeat 0)) [(0, sp'), (1, sp')] 200 $ "function test " ++ show n' ++ "\nlabel hlt\ngoto hlt"
  it "Return" $ property $
    \(sp, lcl, nlcl, arg, this, that, a) -> let sp' = (sp .&. 0x3fff) + 256
                                                lcl' = lcl .&. 0xffff
                                                nlcl' = nlcl .&. 0xff
                                                arg' = arg .&. 0xffff
                                                this' = this .&. 0xffff
                                                that' = that .&. 0xffff
                                                a' = a .&. 0xffff
                                                goal = [(0, sp'+1), (1, lcl'), (2, arg'), (3, this'), (4, that'), (sp', a')]
                                                mem = [(0, sp'+7+nlcl'), (1, sp'+6), (2, sp')] ++
                                                      [(sp'+1, 3), (sp'+2, lcl'), (sp'+3, arg'), (sp'+4, this'), (sp'+5, that'), (sp'+6+nlcl', a')]
                                                code = "goto label1\nlabel hlt\ngoto hlt\nlabel label1\nreturn"
                                            in testAsm goal mem 200 code

-- |Cast a bit sequence representing a i16 to a machine-length int
fixSign :: Int -> Int
fixSign x = if x .&. 0x8000 /= 0 then x - 0x10000 else x

-- |Assembles a program, sets the given memory addresses, runs the program, then tests that data is in the right place
testAsm :: [(Int, Int)] -> [(Int, Int)] -> Int -> String -> Bool
testAsm goal mem n src = case Asm.resolveLabels =<< Asm.fromString =<< (\xs -> codegen [("Sys.init", 0, zipWith (\i l -> (("main.vm", i), l)) [0..] xs)]) <$> Vm.fromString src of
                              Status (_, Nothing) -> False
                              Status (_, Just x) -> let MachineState m _ _ _ _ = iterate step (setMem mem $ initMachine x) !! n
                                                    in map (\(a, _) -> (a, m!a)) goal == goal
  where setMem dm (MachineState m p pc a d) = MachineState (m // dm) p pc a d
