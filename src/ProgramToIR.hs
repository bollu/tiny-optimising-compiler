module ProgramToIR where
import Language
import IR 
import qualified Data.Map as M
import Data.Traversable
import Data.Foldable

data Builder = Builder {
  currentBBId :: Int,
  bbs :: [BasicBlock],
  opLabelCount :: M.Map (Label Operand) Int
  bbLabelCount :: M.Map (Label BasicBlock) Int
}

newBuilder :: Builder
newBuilder = Builder {
  currentBBId = 0
  bbs = [newBB]
  opLabelCount = M.empty,
  -- we have one basic block with this label
  bbLabelCount = M.fromList [(bbLabel newBB), 1]
}

-- | update the nth element. very inefficient.
updateIdx :: [a] -> Int -> (a -> a) -> [a]
updateIdx xs f x = take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs


-- | lift an edit of a basic block to the current basic block focused
-- | in the builder
liftBBEdit :: (BB -> BB) -> Builder -> Builder
liftBBEdit f builder = builder {
    bbs = updateIdx (bbs builder) (currentBBId builder) f
}

mkNewBB :: State Builder Int
mkNewBB builder = (length (bbs builder), Builder {
            bbs = bbs ++ [newBB]
}

setBB :: Builder -> Int -> Builder
setBB builder i = builder {
  currentBBId = i
}


appendInst :: LabeledInst -> State Builder ()
appendInst i old = (i, old {
  bbs = (currentBB old) ++ [i]
 })


mkBinOpInst :: Operand -> BinOp ->  Operand -> Inst
mkBinOpInst (lhs Plus rhs) = InstAdd lhs rhs
mkBinOpInst (lhs Sub rhs) = InstSub lhs rhs

buildExpr :: Expr' -> State Builder Operand
buildExpr (EInt _ i) = return $  OpConstant i
buildExpr (ELiteral _ i) = return $ OpLiteral i
buildExpr (EBinOp lhs op rhs) = do
    lhs <- buildExpr lhs
    rhs <- buildExpr rhs
    let inst = (mkBinOpInst lhs op rhs)
    appendInst (nonLabeledInst inst)
    return inst


buildAssign :: Literal -> Expr' -> State Builder Inst
buildAssign lit expr = do
  val <- buildExpr
  return $ Store (OperandLiteral lit) val

buildStmt :: Stmt -> State Builder ()
buildStmt (Assign _ lit expr) = buildAssign lit expr
buildStmt (Define _ lit) = return ()


stmtsToInsts :: Stmts -> [BasicBlock]
stmtsToInsts stmts = bbs $ execState (for_ stmts buildStmt) newBuilder

programToIR :: Program -> IRProgram
programToIR stmts = IRProgram (stmtsToInsts stmts)
