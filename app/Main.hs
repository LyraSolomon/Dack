import qualified Language.HackAsm as Asm
import qualified Language.StackVm as Vm
import Control.Monad.StatusMessage
import Control.Monad (zipWithM)
import Text.IntFormats
-- import System.IO
import Data.List (intercalate, isSuffixOf)
import Data.Strings (strPadLeft)
import System.Environment (getArgs)

main :: IO ()
main = do
  files <- getArgs
  if all (".vm" `isSuffixOf`) files && not (null files)
    --then runOutput show (Vm.toAsm <$> Vm.fromString contents)
    then do contents <- mapM readFile files
            let info = zipWithM (\a b -> (,) a <$> Vm.fromString b) files contents
            putStrLn $ showStatus id (Vm.toAsm =<< info)
  else if all (".asm" `isSuffixOf`) files && length files == 1
    then do contents <- readFile $ head files
            putStrLn $ showStatus (showMany $ formatNumber Binary) (Asm.fromString contents >>= Asm.toMachineLang)
  else putStrLn "Input must be a list of .vm files, or a single .asm file"

--runOutput :: (a -> String) -> StatusMessage [a] -> IO ()
--runOutput _ (Status (e, Nothing)) = hPutStr stderr e
--runOutput f (Status (e, Just xs)) = hPutStr stderr e >> (putStrLn . intercalate "\n" . map f) xs
showStatus :: (a -> String) -> StatusMessage a -> String
showStatus _ (Status (e, Nothing)) = e
showStatus f (Status (e, Just x)) = e ++ '\n' : f x

showMany :: (a -> String) -> [a] -> String
showMany f x = intercalate "\n" $ map f x

formatNumber :: IntFormat -> Int -> String
formatNumber Binary = strPadLeft '0' 16 . showInt False Binary
formatNumber Decimal = showInt True Decimal
formatNumber Octal = strPadLeft '0' 6 . showInt False Octal
formatNumber f = strPadLeft '0' 4 . showInt False f
