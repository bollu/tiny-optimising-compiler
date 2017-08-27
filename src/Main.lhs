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
import TransformConstantFolding
import TransformCanonicalizeForMIPS
import PrettyUtils
import qualified OrderedMap as M
import qualified MIPSAsm as MIPS


compileProgram :: Lang.Program a ->  IR.IRProgram
compileProgram p = undefined

pipeline :: [(String, IR.IRProgram -> IR.IRProgram)]
pipeline = [("original", id),
            ("mem2reg", transformMem2Reg),
            ("constant fold", transformConstantFold),
            ("Canonicalization for MIPS", transformCanonicalizeForMIPS)]

runPasses :: [(String, IR.IRProgram -> IR.IRProgram)] -- ^ Pass pipeline
    -> IR.IRProgram -- ^ Current program 
    -> IO IR.IRProgram -- ^ Final program
runPasses [] p = return p
runPasses ((name, pass):passes) p = do
    let p' = pass p
    putStrLn . docToString $ pretty "#  Running pass " <+> 
                             pretty name
    putStrLn . prettyableToString $ p'
    putStrLn . docToString $ pretty "- Value:" <+> pretty (runProgram p')
    runPasses passes p'


main :: IO ()
main = do
     args <- getArgs
     input <- readFile (args !! 0)
     case parseProgram input of
        Left err -> putStrLn err
        Right program -> do
            putStrLn "*** Program:"
            putStrLn . prettyableToString $  program

            let irprogram =  programToIR program
            finalProgram <- runPasses pipeline irprogram

            putStrLn "*** MIPS assembly *** "
            -- putStrLn . docToString . MIPS.unASMDoc . MIPS.generateASM $  finalProgram
\end{code}
