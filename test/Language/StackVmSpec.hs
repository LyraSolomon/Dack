module Language.StackVmSpec (spec) where

import Test.Hspec
import qualified Language.StackVm.BasicTest as BasicTest
import qualified Language.StackVm.PointerTest as PointerTest
import qualified Language.StackVm.StaticTest as StaticTest
import qualified Language.StackVm.SimpleAdd as SimpleAdd
import qualified Language.StackVm.StackTest as StackTest
import qualified Language.StackVm.BasicLoop as BasicLoop
import qualified Language.StackVm.FibonacciSeries as FibonacciSeries
import qualified Language.StackVm.SimpleFunction as SimpleFunction
import qualified Language.StackVm.NestedCall as NestedCall
import qualified Language.StackVm.FibonacciElementMain as FibonacciElementMain
import qualified Language.StackVm.FibonacciElementSys as FibonacciElementSys
import qualified Language.StackVm.StaticsClass1 as StaticsClass1
import qualified Language.StackVm.StaticsClass2 as StaticsClass2
import qualified Language.StackVm.StaticsClassSys as StaticsClassSys
import qualified Language.HackAsm.Eval as EvalAsm
import qualified Language.StackVm.Eval as EvalVm
import qualified Language.HackAsm as Asm
import qualified Language.StackVm as Vm
import Control.Monad.StatusMessage
import Data.Array ((!))

-- |Integration tests for "Language.StackVm"
spec :: Spec
spec = do
  describe "Sample programs" $ do
    context "Memory Access" $ do
      context "Basic Test" $
        let goal = [(256, 472), (300, 10), (401, 21), (402, 22), (3006, 36), (3012, 42), (3015, 45), (11, 510)]
            vals = wrap 3 1 [(0, 256), (1, 300), (2, 400), (3, 3000), (4, 3010)] BasicTest.vals
        in it "VM Emulator" (testVm goal 50 [("main.vm", vals)]) >> it "ASM Emulator" (testAsm goal 500 [("main.vm", vals)])
      context "Pointer Test" $
        let goal = [(256, 6084), (3, 3030), (4, 3040), (3032, 32), (3046, 46)]
            vals = wrap 0 0 [(0, 256)] PointerTest.vals
        in it "VM Emulator" (testVm goal 30 [("main.vm", vals)]) >> it "ASM Emulator" (testAsm goal 300 [("main.vm", vals)])
      context "Static Test" $
        let goal = [(256, 1110)]
            vals = wrap 0 0 [(0, 256)] StaticTest.vals
        in it "VM Emulator" (testVm goal 30 [("main.vm", vals)]) >> it "ASM Emulator" (testAsm goal 300 [("main.vm", vals)])
    context "Stack Arithmetic" $ do
      context "Simple Add" $
        let goal = [(0, 257), (256, 15)]
            vals = wrap 0 0 [(0, 256)] SimpleAdd.vals
        in it "VM Emulator" (testVm goal 30 [("main.vm", vals)]) >> it "ASM Emulator" (testAsm goal 300 [("main.vm", vals)])
      context "Stack Test" $
        let goal = [(0, 266), (256, 0xffff), (257, 0), (258, 0), (259, 0), (260, 0xffff), (261, 0), (262, 0xffff), (263, 0), (264, 0), (265, 0x10000-91)]
            vals = wrap 0 0 [(0, 256)] StackTest.vals
        in it "VM Emulator" (testVm goal 60 [("main.vm", vals)]) >> it "ASM Emulator" (testAsm goal 1000 [("main.vm", vals)])
    context "Program Flow" $ do
      context "Basic Loop" $
        let goal = [(0, 257), (256, 6)]
            vals = wrap 1 1 [(0, 256), (1, 300), (2, 400), (400, 3)] BasicLoop.vals
        in it "VM Emulator" (testVm goal 60 [("main.vm", vals)]) >> it "ASM Emulator" (testAsm goal 600 [("main.vm", vals)])
      context "Fibonacci Series" $
        let goal = [(3000, 0), (3001, 1), (3002, 1), (3003, 2), (3004, 3), (3005, 5)]
            vals = wrap 3 1 [(0, 256), (1, 300), (2, 400), (400, 6), (401, 3000)] FibonacciSeries.vals
        in it "VM Emulator" (testVm goal 90 [("main.vm", vals)]) >> it "ASM Emulator" (testAsm goal 1000 [("main.vm", vals)])
    context "Function Calls" $ do
      context "Simple Function" $
        let goal = [(0, 311), (1, 305), (2, 300), (3, 3010), (4, 4010), (310, 1196)]
            vals = "function Sys.init 0" ++ initRam [(0, 310), (1, 305), (2, 300), (3, 3010), (4, 4010)] ++ "push constant 1234\npush constant 37\n" ++
                   "call SimpleFunction.test 2\nlabel test.hlt\ngoto test.hlt\n" ++ SimpleFunction.vals
        in it "VM Emulator" (testVm goal 30 [("main.vm", vals)]) >> it "ASM Emulator" (testAsm goal 400 [("main.vm", vals)])
      context "Nested Call" $
        let goal = [(0, 261), (1, 261), (2, 256), (3, 4000), (4, 5000), (5, 135), (6, 246)]
            vals = "function Sys.init 0" ++ initRam ([(0, 261), (1, 261), (2, 256), (3, -3), (4, -4), (5, -1), (6, -1),
                                                      (256, 1234), (257, -1), (258, -2), (259, 03), (260, -4)] ++ (zip [261..299] $ repeat (-1))) ++ NestedCall.vals
        in it "VM Emulator" (testVm goal 180 [("main.vm", vals)]) >> it "ASM Emulator" (testAsm goal 2000 [("main.vm", vals)])
      context "Fibonacci Element" $
        let goal = [(0, 262), (261, 3)]; vals = [("main.vm", FibonacciElementMain.vals), ("sys.vm", FibonacciElementSys.vals)]
        in it "VM Emulator" (testVm goal 200 vals) >> it "ASM Emulator" (testAsm goal 2000 vals)
      context "StaticsTest" $
        let goal = [(0, 263), (261, 0x10000-2), (262, 8)]; vals = [("class1.vm", StaticsClass1.vals), ("class2.vm", StaticsClass2.vals), ("sys.vm", StaticsClassSys.vals)]
        in it "VM Emulator" (testVm goal 200 vals) >> it "ASM Emulator" (testAsm goal 2000 vals)

-- |Given a sequence of instructions and starting memory, create a program that wraps it in a function
wrap :: Int -> Int -> [(Int, Int)] -> String -> String
wrap arg lcl mem code = "function Sys.init 0\n" ++ concat (take arg $ repeat "push constant 0\n") ++ "call test " ++ show arg ++ "\nreturn\nfunction test " ++ show lcl ++ 
                        initRam mem ++ code ++ "\nlabel test.hlt\ngoto test.hlt\nreturn"

-- |Get vm instructions that set the given memory values
initRam :: [(Int, Int)] -> String
initRam rs = "\npush constant 0\npop pointer 1\n" ++ concatMap setVal (filter ((/= 4) . fst) rs) ++ concatMap setVal (filter ((== 4) . fst) rs)
  where setVal (i, n) = "push constant " ++ show n ++ "\npop that " ++ show i ++ "\n"

-- |Run a vm program and test that data is in the right place
testVm :: [(Int, Int)] -> Int -> [(String, String)] -> Expectation
testVm goal n src = case Vm.validate =<< mapM (\(s, x) -> (,) s <$> Vm.fromString x) src of
                      Status (e, Nothing) -> e `shouldSatisfy` null
                      Status (_, Just x) -> let EvalVm.MachineState m _ _ = iterate EvalVm.step (EvalVm.initMachine x) !! n
                                            in map (\(a, _) -> (a, m!a)) goal `shouldBe` goal

-- |Run an asm program and test that data is in the right place
testAsm :: [(Int, Int)] -> Int -> [(String, String)] -> Expectation
testAsm goal n src = case Asm.resolveLabels =<< Asm.fromString =<< Vm.toAsm =<< mapM (\(s, x) -> (,) s <$> Vm.fromString x) src of
                       Status (e, Nothing) -> e `shouldSatisfy` null
                       Status (_, Just x) -> let EvalAsm.MachineState m _ _ _ _ = iterate EvalAsm.step (EvalAsm.initMachine x) !! n
                                             in map (\(a, _) -> (a, m!a)) goal `shouldBe` goal
