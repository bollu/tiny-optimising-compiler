module Parser where

import Control.Monad (void)

import Control.Applicative

import Text.Trifecta as TR
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Trifecta.Delta

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token

import Data.ByteString.Char8 as BS

import qualified Text.PrettyPrint.ANSI.Leijen as PP

newtype Literal = Literal { unLiteral :: String }

data BinOp = Plus | Minus | Multiply | Divide | L | G | LEQ | GEQ | EQ

data Expr a = EBinOp a (Expr a) BinOp (Expr a) |
                  EInt a Int |
                  EBool a Bool |
                  ELiteral a Literal

type Expr' = Expr ()

type Block a = [Stmt a]
data Stmt a = If a (Expr a) (Block a) | While a (Expr a) (Block a) | Assign a Literal (Expr a)

type Stmt' = Stmt ()


type Program a = [Stmt a]
type Program' = Program ()

litp :: Parser Literal
litp = do
    c <- lower
    rest <- many (alphaNum <|> oneOf ['_', '-'])
    possible_end <- optional (char '?')
    spaces

    let end = 
          case possible_end of 
            Just c -> [c]
            Nothing -> []
    return $ Literal (c:(rest ++ end))


intp :: Parser Int
intp = fromIntegral <$> integer

boolp :: Parser Bool
boolp = ((const True) <$> symbol "true") <|> ((const False) <$> symbol "false")

binopp :: Parser Expr'
binopp = undefined

exprp :: Parser Expr'
exprp = EInt () <$> intp <|> EBool () <$> boolp <|> ELiteral () <$> litp 

ifp :: Parser Stmt'
ifp = do
  symbol "if"
  e <- exprp
  symbol "{"
  stmts <- sepEndBy stmtp (symbol ";")
  symbol "}"
  return $ If () e stmts

whilep :: Parser Stmt'
whilep = do
  symbol "while"
  e <- exprp
  symbol "{"
  stmts <- sepEndBy stmtp (symbol ";")
  symbol "}"
  return $ While () e stmts


assignp :: Parser Stmt'
assignp = do
  symbol "assign"
  name <- litp
  symbol ":="
  rhs <- exprp
  return $ Assign () name rhs


stmtp :: Parser Stmt'
stmtp = ifp <|> whilep <|> assignp

programp :: Parser Program'
programp = sepEndBy1 stmtp (symbol ";") 


-- vLow level interface to trifecta
parseProgram_ :: String -> Result Program'
parseProgram_ string = TR.parseString (spaces *> programp) (Directed (BS.pack string) 0 0 0 0) string



-- v High level interface
type ErrorString = String
parseProgram :: String -> Either ErrorString Program'
parseProgram str = case parseProgram_ str of
                      Success a -> Right a
                      Failure ErrInfo{ _errDoc = e } -> Left (PP.displayS (PP.renderPretty 0.8 80 e) "")
