{-# LANGUAGE GADTs #-}
module IR where
import Data.Text.Prettyprint.Doc as PP
import qualified Language as L
data SSA
data NotSSA

data Label a = Label { unLabel ::  String }
instance Pretty (Label a) where
  pretty (Label s) = pretty s


-- this is wrong, we need OpLabelInst (Label Inst) | OpLabelBB (Label BB)
data Operand = OpConstant Int | OpLiteral L.Literal

instance Pretty Operand where
  pretty (OpConstant i) = pretty i

  pretty (OpLiteral l) = pretty l



data BasicBlock = BasicBlock { bbInsts :: [LabeledInst], bbRet :: RetInst , bbLabel :: Label BasicBlock }
appendBB :: LabeledInst -> BasicBlock -> BasicBlock
appendBB i bb = bb {
  bbInsts = bbInsts bb ++ [i]
 }

newBB :: BasicBlock
newBB = BasicBlock [] Terminal (Label "undefined")

instance Pretty BasicBlock where
  pretty (BasicBlock insts ret label) = 
    pretty label <> pretty ":" <> line <> (nest 4 body) where
      body = vcat (map (<> semi)
                        ((map pretty insts) ++ [pretty ret]))

data Inst where
  InstAdd :: Operand -> Operand -> Inst
  InstSub :: Operand -> Operand -> Inst
  InstLoad :: Operand -> Inst 
  InstStore :: Operand -> Operand -> Inst 

instance Pretty Inst where
  pretty (InstAdd l r) = pretty "add" <+> pretty l <+> pretty r
  pretty (InstSub l r) = pretty "add" <+> pretty l <+> pretty r
  pretty (InstLoad op) = pretty "load" <+> pretty op
  pretty (InstStore slot val) = pretty "store" <+> pretty val <+> pretty "in" <+> pretty slot


data LabeledInst = LabeledInst { unlabelInst :: Inst, getLabel :: Maybe (Label Inst) }
nonLabeledInst :: Inst -> LabeledInst
nonLabeledInst i = LabeledInst i Nothing

labeledInst :: Inst -> Label Inst -> LabeledInst
labeledInst i label = LabeledInst i (Just label)

instance Pretty (LabeledInst) where
  pretty (LabeledInst i l) = pretty l <+> pretty ":=" <+> pretty i

data RetInst = 
  Break  (Label BasicBlock) | 
  CondJump { jumpOp :: Operand,
             trueLabel :: Label BasicBlock,
             falseLabel :: Label BasicBlock  
           } |
  Terminal

instance Pretty RetInst where
  pretty (Break label) = pretty "break" <+> pretty label
  pretty (Terminal) = pretty "TERMINAL"
  pretty (CondJump op tl fl) =
    pretty "condjump" <+> pretty "true" <> braces (pretty tl) <+>
      pretty "false" <> braces (pretty fl)

newtype IRProgram = IRProgram [BasicBlock]
instance Pretty IRProgram where
  pretty (IRProgram bbs) = vcat (map (\b -> pretty b <> hardline) bbs)
