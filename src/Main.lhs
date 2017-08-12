\begin{code}
module Main where
import Parser
import qualified IR as IR
import IRInterpreter
import qualified Language as Lang
import Data.Text.Prettyprint.Doc
import ProgramToIR
import System.IO
import System.Environment
import TransformMem2Reg
import PrettyUtils
import qualified OrderedMap as M
import qualified MIPSAsm as MIPS


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

            let cfg = mkBBGraph . IR.irProgramBBMap $ irprogram :: CFG

            putStrLn "*** Dom info:"
            let dominatorInfo = constructBBDominators irprogram
            putStrLn . docToString . pretty $ dominatorInfo

            putStrLn "*** Dominator tree: "
            let dominatorTree = constructDominatorTree dominatorInfo (IR.irProgramEntryBBId irprogram)
            putStrLn . prettyableToString $ dominatorTree

            putStrLn "*** Dominance Frontiers: "
            let domfrontiers = fmap (\bbid -> (bbid, getDominanceFrontier dominatorTree cfg bbid))
                                    (M.keys . IR.irProgramBBMap $ irprogram) :: [(IR.BBId, [IR.BBId])]
            putStrLn . docToString $ vcat (fmap pretty domfrontiers)


            putStrLn "*** Mem2Reg ***"
            let mem2regprog = transformMem2Reg irprogram
            putStrLn . prettyableToString $  mem2regprog

            putStrLn "*** Original program value ***"
            putStrLn . prettyableToString . runProgram $ irprogram

            putStrLn "*** Mem2Reg program value ***"
            putStrLn . prettyableToString . runProgram $ mem2regprog

            putStrLn "*** MIPS assembly *** "
            putStrLn . docToString . MIPS.unASMDoc . MIPS.generateASM $  mem2regprog
\end{code}
