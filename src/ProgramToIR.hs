module ProgramToIR where
import Language
import IR 
import qualified Data.Map as M
import Data.Traversable
import Data.Foldable
import Control.Monad.State.Strict

type BBId = Int
data Builder = Builder {
  currentBBId :: BBId,
  bbs :: [BasicBlock]
}

newBuilder :: Builder
newBuilder = Builder {
  currentBBId = 0,
  bbs = [newBB]
}

-- | update the nth element. very inefficient.
updateIdx :: [a] -> Int -> (a -> a) -> [a]
updateIdx xs n f = take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs


liftEditToState :: (s -> s) -> State s ()
liftEditToState f = state $ \s -> ((), f s)

-- | lift an edit of a basic block to the current basic block focused
-- | in the builder
liftBBEdit :: (BasicBlock -> BasicBlock) -> Builder -> Builder
liftBBEdit f builder = builder {
    bbs = updateIdx (bbs builder) (currentBBId builder) f
}

mkNewBB :: State Builder BBId
mkNewBB =  state $ \builder -> (length (bbs builder), Builder {
            bbs = bbs builder ++ [newBB],
            currentBBId = (length (bbs builder))
})

setBB :: Builder -> Int -> Builder
setBB builder i = builder {
  currentBBId = i
}


appendInst :: LabeledInst -> State Builder ()
appendInst i = liftEditToState $ liftBBEdit $ (appendBB i) 


mkBinOpInst :: Operand -> BinOp ->  Operand -> Inst
mkBinOpInst lhs Plus rhs = InstAdd lhs rhs
mkBinOpInst lhs Multiply rhs = InstMul lhs rhs

buildExpr :: Expr' -> State Builder Operand
buildExpr (EInt _ i) = return $  OpConstant i
buildExpr (ELiteral _ i) = return $ OpLiteral i
buildExpr (EBinOp _ lhs op rhs) = do
    lhs <- buildExpr lhs
    rhs <- buildExpr rhs
    let inst = (mkBinOpInst lhs op rhs)
    -- TODO: generate fresh labels
    let label = Label "lbl"
    appendInst (labeledInst inst label)
    return (OpLiteral (Literal (unLabel label)))


buildAssign :: Literal -> Expr' -> State Builder Inst
buildAssign lit expr = do
  val <- buildExpr expr
  let inst = (labeledInst (InstStore (OpLiteral lit) val) (Label ("store" ++ (unLiteral lit))))
  appendInst inst
  return (unlabelInst inst)

buildStmt :: Stmt' -> State Builder ()
buildStmt (Assign _ lit expr) = buildAssign lit expr >> return ()
buildStmt (Define _ lit) = return ()


stmtsToInsts :: [Stmt'] -> [BasicBlock]
stmtsToInsts stmts = bbs $ execState (for_ stmts buildStmt) newBuilder

programToIR :: Program' -> IRProgram
programToIR (Program stmts) = IRProgram (stmtsToInsts stmts)
