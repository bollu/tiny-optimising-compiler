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
import qualified Data.Map as M


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

            putStrLn "*** Children of domtree node: "
            let domsubtree = fmap (\bbid -> (bbid, domTreeSubtree dominatorTree bbid))
                                    (M.keys . IR.irProgramBBMap $ irprogram) :: [(IR.BBId, [IR.BBId])]
            putStrLn . docToString $ vcat (fmap pretty domsubtree)

            putStrLn "*** Dominance Frontiers: "
            let domfrontiers = fmap (\bbid -> (bbid, getDominanceFrontier dominatorTree cfg bbid))
                                    (M.keys . IR.irProgramBBMap $ irprogram) :: [(IR.BBId, [IR.BBId])]
            putStrLn . docToString $ vcat (fmap pretty domfrontiers)

