module Language.HackAsmSpec (spec) where

import Test.Hspec
import Language.HackAsm
import Text.IntFormats
import Data.Strings (strPadLeft)
import Control.Monad.StatusMessage
import qualified Language.HackAsm.AddCmp as AddCmp
import qualified Language.HackAsm.AddSrc as AddSrc
import qualified Language.HackAsm.MaxCmp as MaxCmp
import qualified Language.HackAsm.MaxSrc as MaxSrc
import qualified Language.HackAsm.MaxLCmp as MaxLCmp
import qualified Language.HackAsm.MaxLSrc as MaxLSrc
import qualified Language.HackAsm.RectCmp as RectCmp
import qualified Language.HackAsm.RectSrc as RectSrc
import qualified Language.HackAsm.RectLCmp as RectLCmp
import qualified Language.HackAsm.RectLSrc as RectLSrc
import qualified Language.HackAsm.PongCmp as PongCmp
import qualified Language.HackAsm.PongSrc as PongSrc
import qualified Language.HackAsm.PongLCmp as PongLCmp
import qualified Language.HackAsm.PongLSrc as PongLSrc

-- |Integration tests for "Language.HackAsm"
spec :: Spec
spec = do
  xdescribe "Sample programs" $ do
    it "Add" $
      testData AddSrc.vals AddCmp.vals
    it "Max" $
      testData MaxSrc.vals MaxCmp.vals
    it "MaxL" $
      testData MaxLSrc.vals MaxLCmp.vals
    it "Rect" $
      testData RectSrc.vals RectCmp.vals
    it "RectL" $
      testData RectLSrc.vals RectLCmp.vals
    it "Pong" $
      testData PongSrc.vals PongCmp.vals
    it "PongL" $
      testData PongLSrc.vals PongLCmp.vals
  xdescribe "Semantic errors" $ do
    it "Redefinition" $
      (dropWarn . toMachineLang =<< fromString "(SP)\n@SP\n0;JMP") `shouldBe` Status ("", Nothing)
    it "Large number" $
      (dropWarn . toMachineLang =<< fromString "@999999\nD=A\n@SCREEN\nM=D") `shouldBe` Status ("", Nothing)
    it "Large negative number" $
      (dropWarn . toMachineLang =<< fromString "@-999999\nD=A\n@SCREEN\nM=D") `shouldBe` Status ("", Nothing)

-- |Assembles the given source file and compares it to the desired output
testData :: String -> String -> Expectation
testData src cmp = ((unlines . map (strPadLeft '0' 16 . showInt False Binary)) <$> (dropWarn . toMachineLang =<< fromString src)) `shouldBe` return cmp

-- |Removes all warning and error messages from a 'StatusMessage'
dropWarn :: StatusMessage a -> StatusMessage a
dropWarn (Status (_, a)) = Status ("", a)
