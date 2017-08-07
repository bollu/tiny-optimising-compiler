import Test.Tasty
import Control.Monad
import Test.Tasty.HUnit
import Test.Tasty.Runners

import Data.Ord
import qualified Data.Map as M
import System.IO

import IR
import Parser
import IRInterpreter
import Language
import ProgramToIR
import TransformMem2Reg

import System.Directory
import Data.Either
resources :: IO [(FilePath, String)]
resources = do
      programFiles <- listDirectory "./programs/"
      forM programFiles $ \f -> do
                                contents <- readFile $ "./programs/" ++ f
                                return (f, contents)

mkMem2RegTests :: FilePath -> String -> TestTree
mkMem2RegTests filepath contents = testCase ("running " ++ filepath) $ do
  let mTestMem2Reg = testMem2Reg contents
  case mTestMem2Reg of
    Left e -> assertFailure $  "parse error: " ++ e
    Right (orig, mem2reg) -> orig @=? mem2reg

main :: IO ()
main = do
  filepathsWithContents <- resources
  let mem2regTests = fmap (uncurry mkMem2RegTests) filepathsWithContents
  let mem2regGroup = testGroup "mem2reg" mem2regTests
  defaultMain mem2regGroup


testMem2Reg :: String -> Either String (Maybe Int, Maybe Int)
testMem2Reg s =
 case parseProgram s of
    Left err -> Left err
    Right program -> do
            let orig =  programToIR program
            let mem2reg = transformMem2Reg orig
            return $ (runProgram orig, runProgram mem2reg)

