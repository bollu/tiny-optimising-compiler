module Main where
import Parser
import qualified IR as IR
import qualified Data.Text.Lazy as L
import qualified Language as Lang
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Prettyprint.Doc
import ProgramToIR
import System.IO
import System.Environment
import TransformMem2Reg


docToText :: Doc ann -> L.Text
docToText doc = renderLazy (layoutPretty defaultLayoutOptions doc)

docToString :: Doc ann -> String
docToString = L.unpack . docToText

prettyableToText :: Pretty a => a -> L.Text
prettyableToText a = docToText (pretty a)

prettyableToString :: Pretty a => a -> String
prettyableToString  a = docToString (pretty a)


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
            putStrLn . prettyableToString .  programToIR $ program
            putStrLn "*** Dom info:"
            putStrLn . docToString $ vsep (map pretty (take 10 (dominfo_ . programToIR $ program)))

