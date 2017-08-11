{-# LANGUAGE DeriveAnyClass #-}
module MIPSAsm where
import qualified OrderedMap as M
import Control.Monad.State.Strict
import Data.Traversable
import Data.Foldable
import Control.Applicative
import qualified Data.List.NonEmpty as NE
import IR
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils

-- | Parameter to MIPS Instructions
type MIPSParam = Doc ()

-- | Instruction Name
type MIPSInstName = Doc ()

-- | a Mips instruction
type MIPSInst = Doc ()

data ASMContext = ASMContext {
    instToReg :: M.OrderedMap (Label Inst) MIPSParam,
    insts :: [MIPSInst]
}

initASMContext :: ASMContext
initASMContext = ASMContext mempty mempty



newtype ASMDoc = ASMDoc { unASMDoc :: Doc () }

data Reg = RegTemp Int | RegZero | RegPC | RegHI | RegLO deriving (Eq, Ord)



-- | append a MIPSInst to the ASMContext
appendMIPSInst :: MIPSInst -> State ASMContext ()
appendMIPSInst mi = modify (\ctx -> ctx {insts=(insts ctx) ++ [mi]})

-- | Compile binary operator
compileBinaryOp :: MIPSInstName ->  Value -> Value -> State ASMContext ()
compileBinaryOp name v1 v2 = do
    p1 <- sValueToParam v1
    p2 <- sValueToParam v2
    appendMIPSInst $ name <+> p1 <+> p2

-- | Compile Instruction.
compileInst :: Inst -> State ASMContext ()
compileInst (InstAlloc) = error "alloc should not be present in SSA"
compileInst inst@(InstStore _ _) = error . docToString $
    pretty inst <+> pretty "should not be present in SSA"
compileInst inst@(InstLoad _) = error . docToString $
    pretty inst <+> pretty "should not be present in SSA"
compileInst inst@(InstAdd v1 v2) = 
    compileBinaryOp (pretty "add") v1 v2


-- | compile a ret inst
compileRetInst :: RetInst -> State ASMContext ()
compileRetInst (RetInstRet val) = do
    param <- sValueToParam val
    -- | 1 is the ID of print_int
    appendMIPSInst $ pretty "mov $v0 1"
    appendMIPSInst $ pretty "mov $a0 " <+> param



-- | Construct a `Param` from `Value
valueToParam_ :: ASMContext -> Value -> MIPSParam
valueToParam_ _(ValueConstInt i) = pretty i
valueToParam_ ctx (ValueInstRef name) = 
    case M.lookup name  (instToReg ctx) of
        Just param -> param
        Nothing -> error . docToString $ pretty name <+> pretty "not in instToReg"

-- | State version of `valueToParam_`
sValueToParam :: Value -> State ASMContext MIPSParam
sValueToParam value = gets (\ctx -> valueToParam_ ctx value)

-- | append a label to
appendBBLabel :: Label BasicBlock -> State ASMContext ()
appendBBLabel lbl = appendMIPSInst $ pretty (unLabel lbl) <+> pretty ":"

-- | Compile a Basic Block
compileBB :: BasicBlock -> State ASMContext ()
compileBB bb = do
    appendBBLabel (bbLabel bb)
    for (map namedData (bbInsts bb)) compileInst
    compileRetInst (bbRetInst bb)

-- | Generate ASM
generateASM :: IRProgram -> ASMDoc
generateASM program =
  ASMDoc $ vcat $ insts $ execState (do
    -- | Compile all basic blocks
    for (M.elems (irProgramBBMap program)) compileBB) initASMContext
