\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
module MIPSAsm(MReg(..),
MRegLabel,
MBBLabel,
MBB,
MProgram,
MInst(..),
mkMov,
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

-- | Move into `dest` from `src
mkMov :: MReg -- ^ Destination register
        -> MReg  -- ^ Source register
        -> MInst
mkMov dest src = Madd dest regZero src



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
    Mj MBBLabel |
    Mbeqz MReg  MBBLabel |
    Mbgtz MReg MBBLabel

instance Pretty MTerminatorInst where
    pretty (Mexit) = pretty "# <exit>"
    pretty (Mj dest) = pretty "j" <+> pretty dest
    pretty (Mbeqz cond dest) = pretty "beqz" <+> pretty cond <+> pretty dest
    pretty (Mbgtz cond dest) = pretty "Mbgtz" <+> pretty cond <+> pretty dest

type MBBLabel = Label MBB
type MBB = BasicBlock MInst [MTerminatorInst]
type MProgram = Program MInst [MTerminatorInst]


type MLiveRangeBB =  BasicBlock (Int, MInst) (Int, MTerminatorInst)
  
-- | Print a MIPS program into a Doc. Use this to write it into a file.
-- | **Do not use pretty**, because it prints the entry BB as well.
printMIPSAsm :: MProgram -> Doc ()
printMIPSAsm Program{programBBMap=bbmap} = vsep $ fmap printBB (M.elems bbmap)
    where
        printBB :: MBB -> Doc ()
        printBB (BasicBlock{bbLabel=label, bbInsts=is, bbRetInst=ris}) =
            vcat  $
                [pretty label <> pretty ":", indent 4 $ vcat(map pretty is ++ map pretty ris)]

\end{code}
