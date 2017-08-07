module Parser where

import Language
import Control.Monad (void)

import Control.Applicative
import Data.HashSet as HashSet

import Text.Trifecta as TR
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Trifecta.Delta

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Expression
import Text.Parser.Token (TokenParsing, natural, parens, reserve)
import Text.Parser.Token.Style (emptyOps)


import Data.ByteString.Char8 as BS
import qualified Text.PrettyPrint.ANSI.Leijen as TrifectaPP

-- import Data.Text.Prettyprint.Doc as PP
(<??>) = flip (<?>)

-- | Syntax rules for parsing variable-looking like identifiers.
identStyle :: IdentifierStyle Parser
identStyle = IdentifierStyle
    { _styleName = "variable"
    , _styleStart = lower <|> char '_'
    , _styleLetter = alphaNum <|> oneOf "_'#"
    , _styleReserved = HashSet.fromList ["define", "assign", "if", "else", "return", "*", "+", "<", "&&"]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier }

-- | Parse a variable identifier. Variables start with a lower-case letter or
-- @_@, followed by a string consisting of alphanumeric characters or @'@, @_@.
litp :: Parser Literal
litp = "varname" <??> (Literal <$> (ident identStyle))


intp :: Parser Int
intp = fromIntegral <$> integer

boolp :: Parser Bool
boolp = ((const True) <$> symbol "true") <|> ((const False) <$> symbol "false")

term   :: Parser Expr'
term    =  (Text.Parser.Token.parens exprp
       <|> ELiteral () <$> litp <|> EInt () <$> intp) <?> "simple expression"

table  :: [[Operator Parser Expr']]
table  = [[binary "*" Multiply AssocLeft],
          [binary "+" Plus  AssocLeft], 
          [binary "<" L AssocLeft],
          [binary "&&" And AssocLeft]]

binary :: String -> BinOp -> Assoc -> Operator Parser Expr'
binary name op assoc = Infix p assoc where
    p :: Parser (Expr' -> Expr' -> Expr')
    p = do
          reserve identStyle name
          return $ mkBinopExpr op
    mkBinopExpr :: BinOp -> Expr' -> Expr' -> Expr'
    mkBinopExpr op lhs rhs = EBinOp () lhs op rhs

binopp :: Parser Expr'
binopp = buildExpressionParser table term

exprp :: Parser Expr'
exprp =  binopp

ifp :: Parser Stmt'
ifp = do
  symbol "if"
  e <- exprp
  symbol "{"
  thenstmts <- sepEndBy stmtp (symbol ";")
  symbol "}"
  symbol "else"

  symbol "{"
  elsestmts <- sepEndBy stmtp (symbol ";")
  symbol "}"
  return $ If () e thenstmts elsestmts

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

definep :: Parser Stmt'
definep = do
  symbol "define"
  name <- litp
  return $ Define () name

retp :: Parser Stmt'
retp = do
  symbol "return"
  retexpr <- exprp
  return $ Return () retexpr

stmtp :: Parser Stmt'
stmtp = ifp <|> whilep <|> assignp <|> definep <|> retp

programp :: Parser Program'
programp = Program <$> sepEndBy1 stmtp (symbol ";")


-- vLow level interface to trifecta
parseProgram_ :: String -> Result Program'
parseProgram_ string = TR.parseString (spaces *> programp) (Directed (BS.pack string) 0 0 0 0) string



-- v High level interface
type ErrorString = String
parseProgram :: String -> Either ErrorString Program'
parseProgram str = case parseProgram_ str of
                      Success a -> Right a
                      Failure ErrInfo{ _errDoc = e } -> Left (TrifectaPP.displayS (TrifectaPP.renderPretty 0.8 80 e) "")
