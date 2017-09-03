<h1> Transform Pass: IR Canonicalization for MIPS </h1>


<h2> Introduction </h2>

<h4> In this pass, we rewrite binary instructions of the form: </h4>
- `<inst> <int> <ref>`
- `<inst> <ref> <int>`
to
```
<inst> <int> <ref>
```


<h4> We assume that constant folding has already taken place,
so we cannot have: </h4>
```
<inst> <int> <int>
```

<h4> We will leave: </h4>
- `<inst> <ref> <ref>`
as-is.


\begin{code}
{-# LANGUAGE ViewPatterns #-}

module TransformIRToMIPS where
import qualified OrderedMap as M
import TransformMem2Reg (mkCFG, CFG)
import Control.Monad.State.Strict
import Data.Traversable
import Data.Foldable
import Control.Applicative
import qualified Data.List.NonEmpty as NE
import IR
import Graph
import BaseIR
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils
import MIPSAsm

-- | Convert a label of an instruction to a virtual register.
lblToReg :: Label Inst -> MReg
lblToReg lbl = MRegVirtual (unsafeTransmuteLabel lbl)

-- | Create a MachineInst for those IR instructions which have two equivalent
-- | MachineInsts:
-- | One that can take an immediate mode `Int` value, and another that takes
-- | two registers
mkMInstForBinOpFromVariants :: 
    Label Inst -- ^ Destination name
    -> Value -- ^ 1st binary operand `a'
    -> Value -- ^ 2nd binary operand
    -> (MReg -> MReg -> MReg -> MInst) -- ^ Constructor for the instruction
                                       -- that uses two registers as operands.
    -> (MReg -> MReg -> Int -> MInst) -- ^ Constructor for the instruction that 
                                      -- uses a register and an immediate value.
    -> MInst
mkMInstForBinOpFromVariants dstlbl (ValueConstInt i) (ValueInstRef v) _ cimm = 
    cimm (lblToReg dstlbl) (lblToReg v) i

mkMInstForBinOpFromVariants dstlbl (ValueInstRef v) (ValueConstInt i) _ cimm = 
    cimm (lblToReg dstlbl) (lblToReg v) i 

mkMInstForBinOpFromVariants dstlbl (ValueInstRef v) (ValueInstRef v') creg _ = 
    creg (lblToReg dstlbl) (lblToReg v) (lblToReg v')

mkMInstForBinOpFromVariants dstlbl (ValueConstInt i) (ValueConstInt i') creg _= 
    error . docToString $ vcat  
        [pretty "expected instruction to be constant folded",
         pretty "Found illegal operands:",
         pretty dstlbl <+> pretty ":= f(" <+> 
         pretty i <+> pretty "," <+> pretty i <+> pretty ")"]


-- | Transform an `Inst` to a sequence of `MInst`
transformInst :: Named Inst -> [MInst] 
transformInst (Named dest (InstAdd a b)) =
    [mkMInstForBinOpFromVariants dest a b Madd Maddi]

transformInst (Named dest (InstL a b)) =
    [mkMInstForBinOpFromVariants dest a b Mslt Mslti]

-- | Note that for now, we assume that multiplication never happens between 
-- | constants.
transformInst (Named dest (InstMul (ValueInstRef a) (ValueInstRef b))) =
    [Mmult (lblToReg a) (lblToReg b), Mmflo (lblToReg dest)]

-- | A phi node is simply "coalesced" in the preceding basic blocks.
-- | @see emitFusePhi, transformBB.
transformInst (Named _ (InstPhi _)) = []

transformInst inst =
    error . docToString $ pretty "unimplemented lowering for Inst: " <+>
        pretty inst


-- | Make a MInst that sets a MReg (which _must_ be a real register) to a value.
mkMInstSetRealRegToValue :: MReg -- ^ Register to set
                        -> Value -- ^ Value to use the register to
                        -> MInst
mkMInstSetRealRegToValue (MRegReal name) (ValueConstInt i) = 
    Mli (MRegReal name) i
mkMInstSetRealRegToValue (MRegReal name) (ValueInstRef lbl) = 
    mkMov (MRegReal name) (lblToReg lbl)

-- | Code needed in $v0 to issue "print integer".
codePrintInt :: Int
codePrintInt = 1

-- | Code needed in $v0 to issue exit.
codeExit :: Int
codeExit = 10

-- | Transform a `RetInst` into possible `MInsts` and a terminator inst.
transformRetInst :: RetInst -> ([MInst], [MTerminatorInst])
transformRetInst (RetInstRet v) = 
    ([mkMInstSetRealRegToValue rega0 v,
      Mli regv0 codePrintInt,
      Msyscall,
      Mli regv0 codeExit,
      Msyscall],
      [Mexit])

transformRetInst (RetInstBranch lbl) = 
    ([], [Mbeqz regZero (unsafeTransmuteLabel lbl)])

transformRetInst (RetInstConditionalBranch 
    (ValueInstRef (unsafeTransmuteLabel -> condlbl))
    (unsafeTransmuteLabel -> thenlbl)
    (unsafeTransmuteLabel -> elselbl)) = 
        ([], 
        [Mbgtz (MRegVirtual condlbl) thenlbl,
         Mj elselbl])

-- | Shortcut a jump from a branch of "0" to a direct jump
-- | Note that these should ideally be fused in a previous pass
-- | TODO: implement BB fusion.
transformRetInst (RetInstConditionalBranch 
    (ValueConstInt 0)
    _
    (unsafeTransmuteLabel -> elselbl)) = 
        ([], 
        [Mj elselbl])

transformRetInst retinst = 
    error . docToString $ pretty "unimplemented lowering for RetInst: " <+>
        pretty retinst

-- | Emit code such that if the current basic block jumps to a basic block
-- | that has a phi node, we write to a register that the phi node would have
-- | occupied.
emitFusePhi :: CFG -> IRProgram -> IRBBId -> [MInst]
emitFusePhi cfg Program{programBBMap=bbmap} curbbid =
    let
        -- | Make an instruction that creates a `mov' into the phi node
        -- | register from the source register.
        mkMovForPhi :: (Label Inst, Label Inst) -> MInst
        mkMovForPhi (phiname, srcname) = mkMov
            (MRegVirtual(unsafeTransmuteLabel phiname))
            (MRegVirtual(unsafeTransmuteLabel srcname))
    in map mkMovForPhi succPhiReferences
    where
        -- | BBs that are successors in the CFG
        succbbs :: [IRBB]
        succbbs = fmap (bbmap M.!) (getImmediateChildren cfg curbbid)
        -- | Phi nodes of all successor basic blocks
        succphis :: [Named Inst]
        succphis = succbbs >>= getIRBBPhis
        -- | Names of variables referred to by successors of current BB
        -- | LHS is the phi node name.
        -- | RHS is the source inst name.
        succPhiReferences :: [(Label Inst, Label Inst)]
        succPhiReferences = succphis >>= \(Named phiname phi) -> 
            case getPhiValueForBB curbbid phi of
                Just (ValueInstRef instname) -> [(phiname, instname)]
                _ -> []
      


-- | Transform an IR basic block to a machine Basic Block
transformBB :: CFG -> IRProgram -> IRBB -> MBB
transformBB cfg program (bb@BasicBlock {
        bbInsts=insts,
        bbRetInst=retinst,
        bbLabel=curbbid
    }) = BasicBlock {
        bbLabel=unsafeTransmuteLabel curbbid,
        bbInsts=insts' ++ instsFromSucceedingPhi ++ instsFromRet,
        bbRetInst=retinst'
    } where
        insts' = insts >>= transformInst
        (instsFromRet, retinst') = transformRetInst retinst
        instsFromSucceedingPhi = emitFusePhi cfg program curbbid

\end{code}

SPIM assumes that our entry label is called `main`. To stick to the convention,
we re-label our entry basic block to `main`.

\begin{code}
-- Rename the entry BB to "main"
renameEntryBlockToMain :: IRProgram -> IRProgram
renameEntryBlockToMain p@Program {
  programBBMap=bbmap,
  programEntryBBId=entrybbid
} = mapProgramBBs (mapBB id (mapRetInstBBId setEntryToMain)) p' where
    entryBB :: IRBB
    entryBB = (bbmap M.! entrybbid) {
        bbLabel=Label "main"
    }

    -- | bbmap with entry block changed to "main"
    bbmap' = M.insert (Label "main") entryBB 
                    (M.delete entrybbid bbmap)

    -- | IRProgram with the entry block edited to be "main"
    p' :: IRProgram
    p' = Program {
        programEntryBBId=Label "main",
        programBBMap = bbmap'
    }
    -- | Rewrite the "entry" BBId to "main".
    setEntryToMain :: IRBBId -> IRBBId
    setEntryToMain lbl = if lbl == entrybbid then Label "main" else lbl

\end{code}

Finally, we write the interface to our transformation as a function
`transformIRToMIPS`.
\begin{code}
transformIRToMIPS :: IRProgram -> MProgram
transformIRToMIPS irprogram = 
    mapProgramBBs (transformBB cfg irprogram) (renameEntryBlockToMain irprogram) where
        cfg = mkCFG (programBBMap irprogram)
\end{code}

