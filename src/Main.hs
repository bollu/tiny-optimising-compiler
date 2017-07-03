module Main where
import Parser
import qualified IR as IR
import qualified Language as Lang
import Data.Text.Prettyprint.Doc
import ProgramToIR
import System.IO
import System.Environment
import TransformMem2Reg
import PrettyUtils


compileProgram :: Lang.Program a ->  IR.IRProgram
compileProgram p = undefined

main :: IO ()
main = do
     args <- getArgs
     input <- readFile (args !! 0)
     case parseProgram input of
        Left err -> putStrLn err
        Right program -> do
            putStrLn "*** Program:"
            putStrLn . prettyableToString $  program

            putStrLn "*** IR:"
            let irprogram =  programToIR program
            putStrLn . prettyableToString $ irprogram

            putStrLn "*** Dom info:"
            let dominatorInfo = constructBBDominators irprogram
            putStrLn . docToString . pretty $ dominatorInfo

            putStrLn "*** Dominator tree: "
            let dominatorTree = constructDominatorTree dominatorInfo (IR.irProgramEntryBBId irprogram)
            putStrLn . prettyableToString $ dominatorTree

