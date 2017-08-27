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
import TransformConstantFolding

import Control.Monad(filterM)
import Data.List(permutations)

import Data.Foldable(forM_)

import System.Directory
import Data.Either

-- | collection of all program paths and their contents
resources :: IO [(FilePath, String)]
resources = do
      programFiles <- listDirectory "./programs/"
      forM programFiles $ \f -> do
                                contents <- readFile $ "./programs/" ++ f
                                return (f, contents)

-- | Make a test case for a *single* pass
mkPassTest :: FilePath -> String -> Pass -> TestTree
mkPassTest filepath contents pass@(passname, _) =
    -- | use testCaseSteps so we can print errors on parsing and reference program evaluation.
    testCaseSteps (passname) $ \step ->  do
      parseSourceToIR step contents $ \seedir -> do
           v <- runReferenceProgram step seedir
           verifyPass pass seedir v

-- | Make a test case for all passes to run on this particular file
mkAllPassesTests :: FilePath -> String -> TestTree
mkAllPassesTests filepath contents =
    let
        tests = map (mkPassTest  filepath contents) allPasses
    in testGroup (filepath) tests

main :: IO ()
main = do
  filepathsWithContents <- resources
  let tests = fmap (uncurry mkAllPassesTests) filepathsWithContents
  let group = testGroup "All passes on all files" tests
  defaultMain group

--  | An IR pass
type Pass = (String, IRProgram -> IRProgram)

-- | A list of basic passes.
basicPasses :: [Pass]
basicPasses = [("mem2reg", transformMem2Reg),
               ("constant fold", transformConstantFold)]


-- | Get all powersets which are not null.
nonNullPowerset_ :: [a] -> [[a]]
nonNullPowerset_ xs = filter (not . null) (filterM (const [True, False]) xs)

-- | Compose a list of passes into one pass
composePassList_ :: [Pass] -> Pass
composePassList_ ((xs, xf):ls) =
    let (ys, yf) = composePassList_ ls
        name = if ys == "" then xs else xs ++ ", " ++ ys
    in (name, yf . xf)
composePassList_ [] = ("", id)

-- | Compose all passes in all possible orders of all subsets.
allPasses :: [Pass]
allPasses = nonNullPowerset_ basicPasses >>= \subset ->
                composePassList_ <$> permutations subset

-- | Parse a program
parseSourceToIR :: (String -> IO ()) -- Function to log messages
                      -> String  -- Source program
                      -> (IRProgram -> Assertion) -- Parse success handler
                      -> Assertion
parseSourceToIR step raw cont =  do
    step $ "Parsing source program..."
    case parseProgram raw of
        Left e -> assertFailure $ "parse error: " ++ e
        Right p -> cont (programToIR p)

-- | Run the reference program
-- | TODO: once the IR interpreter can throw errors, catch the error here.
runReferenceProgram :: (String -> IO ()) -> IRProgram ->  IO (Maybe Int)
runReferenceProgram step seedir = do
    step $ "Running source program IR..."
    return $ runProgram seedir


-- | Verify that a pass is correct
verifyPass :: Pass -- The pass to run
              -> IRProgram  -- The source program
              ->  Maybe Int -- The correct value expected
              -> Assertion
verifyPass (passname, passfn) seedir expectedVal = do
    expectedVal @=? runProgram (passfn $ seedir)


-- parseAndRunPasses :: String -> Either String (Maybe Int, [(String, Maybe Int)])
-- parseAndRunPasses s =
--  case parseProgram s of
--     Left err -> Left err
--     Right program -> do
--             let seedIR = programToIR program
--             let transformedPrograms = map (\(name, p) -> (name, p seedIR)) allPasses :: [(String, IRProgram)]
--             return $ (runProgram seedIR,
--                       map (\(name, p) -> (name, runProgram p))  transformedPrograms)
--
