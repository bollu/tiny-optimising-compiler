module Main where
import Parser
import qualified IR as IR
import qualified Data.Text.Lazy as L
import qualified Language as Lang
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Prettyprint.Doc


prettyToText :: Doc ann -> L.Text
prettyToText doc = renderLazy (layoutPretty defaultLayoutOptions doc)

compileProgram :: Lang.Program a ->  IR.IRProgram
compileProgram p = undefined

main :: IO ()
main = do
     input <- readFile "programs/input.c"
     case parseProgram input of
        Left err -> putStrLn err
        Right val -> putStrLn . L.unpack . prettyToText . pretty $  val

