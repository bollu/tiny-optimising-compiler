module Language where
import Data.Text.Prettyprint.Doc as PP

newtype Literal = Literal { unLiteral :: String }
instance Pretty Literal where
  pretty = pretty . unLiteral

data BinOp = Plus | Minus | Multiply | Divide | L | G | LEQ | GEQ | EQ
instance Pretty BinOp where
  pretty Plus = pretty "+"
  pretty Minus = pretty "-"
  pretty Multiply = pretty "*"
  pretty Divide = pretty "/"
  pretty L = pretty "<"
  pretty G = pretty ">"
  pretty LEQ = pretty "<="
  pretty GEQ = pretty ">="
  pretty Language.EQ = pretty "=="

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
data Stmt a = If a (Expr a) (Block a) |
              While a (Expr a) (Block a) |
              Assign a Literal (Expr a) |
              Define a Literal


nestDepth :: Int
nestDepth = 4

instance Pretty (Stmt a) where
  pretty (If _ cond then') = pretty "if" <+> pretty cond <+> PP.braces (nest 4 (pretty then'))
  pretty (While _ cond body) = pretty "while" <+> pretty cond <+> PP.braces (nest 4 (pretty body))
  pretty (Assign _ lhs rhs) = pretty "assign" <+> pretty lhs <+> pretty ":=" <+> pretty rhs
  pretty (Define _ lit) = pretty "define" <+> pretty lit

type Stmt' = Stmt ()


newtype Program a = Program [Stmt a]
type Program' = Program ()

instance Pretty (Program a) where
  pretty (Program stmts) = vcat (map pretty stmts)
