module ProgramToIR where
import Language
import IR 
import qualified Data.Map as M
import Data.Traversable
import Data.Foldable
import Control.Monad.State.Strict

data Builder = Builder {
  currentBBId :: BBId,
  -- | List of basic blocks
  bbs :: [BasicBlock],
  -- | counter to generate new instruction name
  tmpInstNamesCounter :: Int,
  -- | Map from literal name to Value
  literalToValue :: M.Map Literal Value
}

-- | Create a new builder with an empty basic block
newBuilder :: Builder
newBuilder = Builder {
  currentBBId = 0,
  bbs = [defaultBB],
  tmpInstNamesCounter=0,
  literalToValue=mempty
}

-- | Focus the basic block given by the ID
focusBB :: BBId -> State Builder ()
focusBB id = modify (\b-> b { currentBBId=id })

-- | Append a new basic block. DOES NOT switch the currentBBId to the new basic block. For that, see focusBB
createNewBB :: Label Builder -> State Builder BBId
createNewBB name = do
  nbbs <- gets (length . bbs)
  let nameunique = Label ((unLabel name) ++ "." ++ show nbbs)
  let namedbb = defaultBB { bbLabel=nameunique }
  curbbs <- gets bbs
  modify (\b -> b { bbs = curbbs ++ [namedbb]} )
  return nbbs

-- | Update the nth element. very inefficient.
updateIdx :: [a] -> Int -> (a -> a) -> [a]
updateIdx xs n f = take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs


-- | Create a temporary instruction name.
getTempInstName :: State Builder (Label Inst)
getTempInstName = do
  n <- gets tmpInstNamesCounter
  modify (\b -> b { tmpInstNamesCounter=n+1 })
  return . Label $ "tmp." ++ show n


-- | Create a temporary name for a return instruction
-- | Note that we cheat in the implementation, by just "relabelling"
-- | an instruction label to a ret instruction label.
getTempRetInstName :: State Builder (Label RetInst)
getTempRetInstName = Label . unLabel <$> getTempInstName

-- | Add a mapping between literal and value.
mapLiteralToValue :: Literal -> Value -> State Builder ()
mapLiteralToValue l v = do
  ltov <- gets literalToValue
  -- TODO: check that we do not repeat literals.
  modify (\b -> b { literalToValue=M.insert l v ltov })
  return ()

-- | Get the value that the Literal maps to.
getLiteralValueMapping :: Literal -> State Builder Value
getLiteralValueMapping lit = do
  ltov <- gets literalToValue
  return $ ltov M.! lit

-- | lift an edit of a basic block to the current basic block focused
-- | in the Builder.
liftBBEdit :: (BasicBlock -> BasicBlock) -> Builder -> Builder
liftBBEdit f builder = builder {
    bbs = updateIdx (bbs builder) (currentBBId builder) f
}

-- | Set the builder's current basic block to the i'th basic block
setBB :: Builder -> Int -> Builder
setBB builder i = builder {
  currentBBId = i
}


-- | Append instruction "I" to the builder
appendInst :: Named Inst -> State Builder Value
appendInst i = do
  modify . liftBBEdit $ (appendInstToBB i) 
  return $ ValueInstRef (namedName i)
  where
    appendInstToBB :: Named Inst -> BasicBlock -> BasicBlock
    appendInstToBB i bb = bb { bbInsts=bbInsts bb ++ [i] }

setRetInst :: RetInst -> State Builder ()
setRetInst i = do
  modify . liftBBEdit $ (setBBRetInst i) 
  where
    setBBRetInst :: RetInst -> BasicBlock -> BasicBlock
    setBBRetInst i bb = bb { bbRetInst=i }


mkBinOpInst :: Value -> BinOp ->  Value -> Inst
mkBinOpInst lhs Plus rhs = InstAdd lhs rhs
mkBinOpInst lhs Multiply rhs = InstMul lhs rhs
mkBinOpInst lhs L rhs = InstL lhs rhs
mkBinOpInst lhs And rhs = InstAnd lhs rhs

buildExpr :: Expr' -> State Builder Value
buildExpr (EInt _ i) = return $  ValueConstInt i
buildExpr (ELiteral _ lit) = getLiteralValueMapping lit
buildExpr (EBinOp _ lhs op rhs) = do
    lhs <- buildExpr lhs
    rhs <- buildExpr rhs
    let inst = (mkBinOpInst lhs op rhs)
    name <- getTempInstName
    -- TODO: generate fresh labels
    appendInst $ name =:= inst

-- | Build the IR for the assignment, and return a reference to @InstStore
-- | TODO: technically, store should not return a Value
buildAssign :: Literal -> Expr' -> State Builder Value
buildAssign lit expr = do
  exprval <- buildExpr expr
  litval <- getLiteralValueMapping lit
  name <- getTempInstName
  -- TODO: do not allow Store to be named with type system trickery
  appendInst $ name =:= InstStore litval exprval

-- | Build IR for "define x"
buildDefine :: Literal -> State Builder Value
buildDefine lit = do
  name <- getTempInstName
  mapLiteralToValue lit (ValueInstRef name)
  appendInst $ name =:= InstAlloc

-- | Build IR for "Stmt"
buildStmt :: Stmt' -> State Builder ()
buildStmt (Define _ lit) = buildDefine lit >> return ()
buildStmt (Assign _ lit expr) = buildAssign lit expr >> return ()
buildStmt (If _ cond then' else') = do
  condval <- buildExpr cond
  currbb <- gets currentBBId
  
  bbthen <- createNewBB (Label "then")
  focusBB bbthen
  stmtsToInsts then'


  bbelse <- createNewBB (Label "else")
  focusBB bbelse
  stmtsToInsts else'

  bbjoin <- createNewBB (Label "join")

  focusBB bbthen
  thenjump <- getTempInstName
  setRetInst $ RetInstBranch bbjoin

  focusBB bbelse
  setRetInst $ RetInstBranch bbjoin
  
  focusBB currbb
  name <- getTempRetInstName
  setRetInst $ RetInstConditionalBranch condval bbthen bbelse

  focusBB bbjoin

buildStmt (While _ cond body) = do
  curbb <- gets currentBBId
  condbb <- createNewBB "while.cond"
  bodybb <- createNewBB "while.body"
  endbb <- createNewBB "while.end"

  focusBB condbb
  condval <- buildExpr cond
  setRetInst $ RetInstConditionalBranch condval bodybb endbb

  focusBB bodybb
  stmtsToInsts body
  setRetInst $ RetInstBranch condbb

  focusbb curbb
  setRetInst $ RetInstBranch condbb

  focusbb endbb


-- Given a collection of statements, create a State that will create these
-- statements in the builder
stmtsToInsts :: [Stmt'] -> State Builder ()
stmtsToInsts stmts = (for_ stmts buildStmt)


programToIR :: Program' -> IRProgram
programToIR (Program stmts) = IRProgram (bbs $ execState (stmtsToInsts stmts) newBuilder)
