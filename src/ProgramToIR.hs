module ProgramToIR where
import Language
import IR 
import qualified Data.Map.Strict as M
import Data.Traversable
import Data.Foldable
import Control.Monad.State.Strict
import qualified Data.Tree as T

data Builder = Builder {
  -- | The first BB that is present in the module
  entryBBId :: BBId,
 -- | The BB the builder is currently focused on
  currentBBId :: BBId,
  -- | Mapping from BBId to BasicBlock
  bbIdToBB :: M.Map BBId BasicBlock,
  -- | counter to generate new instruction name
  tmpInstNamesCounter :: Int,
  -- | Map from literal name to Value
  literalToValue :: M.Map Literal Value
}

-- | Create a new builder with an empty basic block
newBuilder :: Builder
newBuilder = 
  execState mkDefaultBB initbuilder
    where
      mkDefaultBB = do
        bbid <- createNewBB (Label "default")
        focusBB bbid
        -- Set the "entry" basic block so we can later give it to IRProgram
        modify (\b -> b { entryBBId = bbid })

      initbuilder = (Builder {
        entryBBId = Label "",
        currentBBId = Label "",
        bbIdToBB = M.empty,
        tmpInstNamesCounter=0,
        literalToValue=mempty
    }) 

-- | Get the current Basic block ID
getCurrentBBId :: State Builder BBId
getCurrentBBId = gets currentBBId

-- | Focus the basic block given by the ID
focusBB :: BBId -> State Builder ()
focusBB id = modify (\b-> b { currentBBId=id })

-- | Append a new basic block. DOES NOT switch the currentBBId to the new basic block. For that, see focusBB
createNewBB :: Label Builder -> State Builder BBId
createNewBB name = do
  idtobbs <- gets bbIdToBB
  let nbbs = M.size idtobbs
  let nameunique = Label ((unLabel name) ++ "." ++ show nbbs)
  let newbb = defaultBB { bbLabel=nameunique }
  modify (\b -> b { bbIdToBB = M.insert nameunique newbb idtobbs  } )
  return nameunique


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
    bbIdToBB = M.adjust f (currentBBId builder) (bbIdToBB builder) 
}

-- | Set the builder's current basic block to the i'th basic block
setBB :: Builder -> BBId -> Builder
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
  currbb <- getCurrentBBId
  
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
  curbb <- getCurrentBBId
  condbb <- createNewBB (Label "while.cond")
  bodybb <- createNewBB (Label "while.body")
  endbb <- createNewBB (Label "while.end")

  focusBB condbb
  condval <- buildExpr cond
  setRetInst $ RetInstConditionalBranch condval bodybb endbb

  focusBB bodybb
  stmtsToInsts body
  setRetInst $ RetInstBranch condbb

  focusBB curbb
  setRetInst $ RetInstBranch condbb

  focusBB endbb


-- Given a collection of statements, create a State that will create these
-- statements in the builder
stmtsToInsts :: [Stmt'] -> State Builder ()
stmtsToInsts stmts = (for_ stmts buildStmt)


programToIR :: Program' -> IRProgram
programToIR (Program stmts) = 
  IRProgram {
    irProgramBBMap = bbIdToBB  builder,
    irProgramEntryBBId = entryBBId builder
  } where
      builder = execState (stmtsToInsts stmts) newBuilder


type DominatorTree = T.Tree BBId
