module Language where
import Data.Text.Prettyprint.Doc as PP

newtype Literal = Literal { unLiteral :: String } deriving(Ord, Eq)
instance Pretty Literal where
  pretty = pretty . unLiteral

data BinOp = Plus | Multiply | L | And
instance Pretty BinOp where
  pretty Plus = pretty "+"
  pretty Multiply = pretty "*"
  pretty L = pretty "<"
  pretty And = pretty "&&"

data Expr a = EBinOp a (Expr a) BinOp (Expr a) |
                  EInt a Int |
                  ELiteral a Literal

instance Pretty (Expr a) where
  pretty (EBinOp _ l op r) = pretty "(" <+> pretty op <+>
                             pretty l <+> pretty r <+> pretty ")"
  pretty (EInt _ i) = pretty i
  pretty (ELiteral _ lit) = pretty lit

type Expr' = Expr ()

type Block a = [Stmt a]
data Stmt a = If a (Expr a) (Block a) (Block a) |
              While a (Expr a) (Block a) |
              Assign a Literal (Expr a) |
              Define a Literal


nestDepth :: Int
nestDepth = 4

instance Pretty (Stmt a) where
  pretty (If _ cond then' else') = pretty "if" <+> pretty cond <+> 
                                  PP.braces (nest 4 (pretty then')) <+> 
                                  PP.braces (nest 4 (pretty else'))

  pretty (While _ cond body) = pretty "while" <+> pretty cond <+> PP.braces (nest 4 (pretty body))
  pretty (Assign _ lhs rhs) = pretty "assign" <+> pretty lhs <+> pretty ":=" <+> pretty rhs
  pretty (Define _ lit) = pretty "define" <+> pretty lit

type Stmt' = Stmt ()


newtype Program a = Program [Stmt a]
type Program' = Program ()

instance Pretty (Program a) where
  pretty (Program stmts) = vcat (map pretty stmts)
