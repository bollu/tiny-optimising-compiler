\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
module MIPSAsm(MReg(..),
MRegLabel,
MBBLabel,
MBB,
MProgram,
MInst(..),
MTerminatorInst(..),
regZero,
rega0,
regv0,
printMIPSAsm) where
import qualified OrderedMap as M
import Control.Monad.State.Strict
import Data.Traversable
import Data.Foldable
import Control.Applicative
import qualified Data.List.NonEmpty as NE
import BaseIR
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils


type MRegLabel = Label MReg

-- A register for our machine instructions.
data MReg = MRegVirtual MRegLabel | MRegReal String

regZero :: MReg
regZero = MRegReal "zero"

rega0 :: MReg
rega0 = MRegReal "a0"

regv0 :: MReg
regv0 = MRegReal "v0"


instance Pretty MReg where
    pretty (MRegReal name) = pretty "$" PP.<> pretty name
    pretty (MRegVirtual i) = pretty "$virt" PP.<> pretty i


data MInst where
    Mli :: MReg -> Int -> MInst
    Mmflo :: MReg -> MInst
    Madd :: MReg -> MReg -> MReg -> MInst
    Maddi :: MReg -> MReg -> Int -> MInst
    Mori :: MReg -> MReg -> Int -> MInst
    Mslt :: MReg -> MReg -> MReg -> MInst
    Mslti :: MReg -> MReg -> Int -> MInst
    Mmult :: MReg -> MReg -> MInst
    Msyscall :: MInst


_prettyMBinOp :: (Pretty a, Pretty b, Pretty c) => 
    String -> a -> b -> c -> PP.Doc doc
_prettyMBinOp name a b c = pretty name <+> pretty a <+> pretty b <+> pretty c
instance Pretty MInst where
    pretty (Mli dest val) = pretty "li" <+> pretty dest <+> pretty val
    pretty (Mmflo dest) = pretty "mflo" <+> pretty dest
    pretty (Madd dest a b) = _prettyMBinOp "add" dest a b
    pretty (Maddi dest a b) = _prettyMBinOp "addi" dest a b
    pretty (Mori dest a b) = _prettyMBinOp "ori" dest a b
    pretty (Mslt dest a b) = _prettyMBinOp "slt" dest a b
    pretty (Mslti dest a b) = _prettyMBinOp "slti" dest a b
    pretty (Mmult a b) = pretty"mult" <+> pretty a <+> pretty b
    pretty (Msyscall) = pretty "syscall"

data MTerminatorInst =
    Mexit | 
    Mbeqz MReg  MBBLabel

instance Pretty MTerminatorInst where
    pretty (Mexit) = pretty "# <exit>"
    pretty (Mbeqz dest lbl) = pretty "beqz" <+> pretty dest <+> pretty lbl

type MBBLabel = Label MBB
type MBB = BasicBlock MInst MTerminatorInst
type MProgram = Program MInst MTerminatorInst


type MLiveRangeBB =  BasicBlock (Int, MInst) (Int, MTerminatorInst)
  
-- | Print a MIPS program into a Doc. Use this to write it into a file.
-- | **Do not use pretty**, because it prints the entry BB as well.
printMIPSAsm :: MProgram -> Doc ()
printMIPSAsm Program{programBBMap=bbmap} = vsep $ fmap pretty (M.elems bbmap)

{-
-- This module assumes that constants in all parameters are canonicalized
-- to be the second parameter.

-- | Parameter to MIPS Instructions
type MIPSParam = Doc ()

-- | Instruction Name
type MIPSInstName = Doc ()

-- | a Mips instruction
type MIPSInst = Doc ()

-- | a mips register name
type MIPSRegName = Doc ()

-- | a mips label
type MIPSLabel = Doc ()


data ASMContext = ASMContext {
    instToReg :: M.OrderedMap (Label Inst) MIPSRegName,
    insts :: [MIPSInst],
    irprogram :: IRProgram
}

initASMContext :: IRProgram -> ASMContext
initASMContext program = ASMContext {
  instToReg=mempty,
  insts=mempty,
  irprogram=program
}

newtype ASMDoc = ASMDoc { unASMDoc :: Doc () }

-- | The zero register
zeroReg :: MIPSRegName
zeroReg = pretty "$zero"

-- | Compile an instruction that sets a particular register value
compileSetRegisterValue :: MIPSRegName  -> MIPSParam -> State ASMContext ()
compileSetRegisterValue regname param =
    appendMIPSInst $ pretty "ori" <+> regname <+> zeroReg <+> param



-- | append a MIPSInst to the ASMContext
appendMIPSInst :: MIPSInst -> State ASMContext ()
appendMIPSInst mi = modify (\ctx -> ctx {insts=(insts ctx) ++ [mi]})

-- | Compile binary operator
compileBinaryOp :: MIPSInstName ->  Value -> Value -> State ASMContext ()
compileBinaryOp name v1 v2 = do
    p1 <- compileValue v1
    p2 <- compileValue v2
    appendMIPSInst $ name <+> p1 <+> p2

-- | Compile Instruction.
appendInst :: Named Inst -> State ASMContext ()
appendInst Named {namedData=InstAlloc} = error "alloc should not be present in SSA"
appendInst Named {namedData=inst@(InstStore _ _)} = error . docToString $
    pretty inst <+> pretty "should not be present in SSA"
appendInst Named {namedData=inst@(InstLoad _)} = error . docToString $
    pretty inst <+> pretty "should not be present in SSA"

-- | compile a ret inst
appendRetInst :: RetInst -> State ASMContext ()
appendRetInst (RetInstRet val) = do
    param <- compileValue val
    -- | 1 is the ID of print_int
    compileSetRegisterValue (pretty "$v0") (pretty (1 :: Int))
    compileSetRegisterValue (pretty "$a0") param
    appendMIPSInst $ pretty "syscall"

    -- | 10 is the ID of terminate
    compileSetRegisterValue (pretty "$v0") (pretty (10 :: Int))
    appendMIPSInst $ pretty "syscall"

appendRetInst (RetInstBranch lbl) = do
    mipslabel <- compileBBLabel lbl
    appendMIPSInst $ pretty "j" <+> mipslabel


appendRetInst (RetInstConditionalBranch cond tlabel flabel) = do
    tmipslabel <- compileBBLabel tlabel
    fmipslabel <- compileBBLabel flabel
    condparam <- compileValue cond
    -- | if cond != 0, goto true
    appendMIPSInst $ pretty "bne" <+> condparam <+> zeroReg <+> tmipslabel
    appendMIPSInst $ pretty "j" <+> fmipslabel

-- | Construct a `Param` from `Value
compileValueRaw_ :: ASMContext -> Value -> MIPSParam
compileValueRaw_ _(ValueConstInt i) = pretty i
compileValueRaw_ ctx (ValueInstRef name) =
    case M.lookup name  (instToReg ctx) of
        Just reg -> reg
        Nothing -> error . docToString $ pretty name <+> pretty "not in instToReg"

-- | State version of `compileValueRaw_`
compileValue :: Value -> State ASMContext MIPSParam
compileValue value = gets (\ctx -> compileValueRaw_ ctx value)

entryBBLabel :: State ASMContext (Label BasicBlock)
entryBBLabel =
    gets (\ctx -> let
                    program :: IRProgram
                    program = irprogram ctx

                    entrybbid :: BBId
                    entrybbid = irProgramEntryBBId program

                    entrybb :: BasicBlock
                    entrybb = (irProgramBBMap program) M.! entrybbid
                  in bbLabel entrybb)

-- | convert a BB label to a MIPSLabel
compileBBLabel :: Label BasicBlock -> State ASMContext (MIPSLabel)
compileBBLabel label = do
    entrylabel <- entryBBLabel
    -- | Rename entry label to `main`
    let label' = if entrylabel == label then (Label "main") else label
    return . pretty . unLabel $ label'

-- | Append a basic block label into the assembly.
appendBBLabel :: Label BasicBlock -> State ASMContext ()
appendBBLabel lbl = do
    mipslbl <- compileBBLabel lbl
    appendMIPSInst $ mipslbl <+> pretty ":"

-- | Compile a Basic Block
compileBB :: BasicBlock -> State ASMContext ()
compileBB bb = do
    appendBBLabel (bbLabel bb)
    for (bbInsts bb) appendInst
    appendRetInst (bbRetInst bb)

-- | Generate ASM
generateASM :: IRProgram -> ASMDoc
generateASM program =
  ASMDoc $ vcat $ insts $ execState (do
    -- | Compile all basic blocks
    for (M.elems (irProgramBBMap program)) compileBB) (initASMContext program)
-}
\end{code}
