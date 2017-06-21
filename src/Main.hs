module Main where
import Parser

main :: IO ()
main = do
     input <- readFile "programs/input.c"
     case parseProgram input of
        Left err -> putStrLn err
        Right val -> putStrLn "success"

