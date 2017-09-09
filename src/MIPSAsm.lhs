\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module MIPSAsm(MReg(..),
MRegLabel,
mkTemporaryReg,
MBBLabel,
MBB,
MProgram,
MInst(..),
mkMov,
regToString,
MTerminatorInst(..),
regZero,
rega0,
regv0,
regsp,
printMIPSAsm,
traverseMInstReg,
mapMInstReg,
foldMapMInstReg,
getMInstRegs,
traverseMTerminatorInstReg,
mapMTerminatorInstReg,
getTerminatorInstSuccessor,
MCFG,
mkMCFG ) where
import qualified OrderedMap as M
import Control.Monad.State.Strict
import Data.Traversable
import Data.Foldable
import Control.Applicative
import qualified Data.List.NonEmpty as NE
import BaseIR
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils
import Data.MonoTraversable
import Data.Functor.Identity(Identity(..), runIdentity)
import qualified Data.Monoid as Monoid
import Graph
import Data.Maybe(maybeToList)



type MRegLabel = Label MReg

-- A register for our machine instructions.
data MReg = MRegVirtual MRegLabel | MRegReal String deriving(Eq, Ord)


-- | Convert from a register to a stringified name.
regToString :: MReg -> String
regToString (MRegVirtual (Label name)) = name
regToString (MRegReal name) = name

regZero :: MReg
regZero = MRegReal "zero"

rega0 :: MReg
rega0 = MRegReal "a0"

regv0 :: MReg
regv0 = MRegReal "v0"

-- | Stack pointer register
regsp :: MReg
regsp = MRegReal "sp"


-- | Count from 0. Make the `n`th temporary register.
-- | There are 8 of these. 
mkTemporaryReg :: Int -> MReg
mkTemporaryReg n =
    if n > 7 || n < 0
    then error . docToString $ pretty "expected 0 <= n <= 7, found:" <+> pretty n
    else MRegReal ("t" ++ show n)


instance Pretty MReg where
    pretty (MRegReal name) = pretty "$" PP.<> pretty name
    pretty (MRegVirtual i) = pretty "$virt-" PP.<> pretty i


data MInst where
    Mli :: MReg -> Int -> MInst
    Mmflo :: MReg -> MInst
    Madd :: MReg -> MReg -> MReg -> MInst
    Maddi :: MReg -> MReg -> Int -> MInst
    Mori :: MReg -> MReg -> Int -> MInst
    Mslt :: MReg -> MReg -> MReg -> MInst
    Mslti :: MReg -> MReg -> Int -> MInst
    Mmult :: MReg -> MReg -> MInst
    -- | Store a register word with an immediate mode offset and a base register.
    Msw :: MReg -> Int -> MReg -> MInst
    -- | Load into a register from a base register plus an immediate mode offset
    Mlw :: MReg -> Int -> MReg -> MInst
    Mcomment :: String -> MInst
    Msyscall :: MInst

-- | Move into `dest` from `src
mkMov :: MReg -- ^ Destination register
        -> MReg  -- ^ Source register
        -> MInst
mkMov dest src = Madd dest regZero src

type instance Element MInst = MReg

instance MonoFunctor MInst where
    omap f (Mli reg i) = Mli (f reg) i
    omap f (Mmflo reg) = Mmflo (f reg)
    omap f (Madd r1 r2 r3) = Madd (f r1) (f r2) (f r3)
    omap f (Maddi r1 r2 i) = Maddi (f r1) (f r2) i
    omap f (Mori r1 r2 i) = Mori (f r1) (f r2) i
    omap f (Mslt r1 r2 r3) = Mslt (f r1) (f r2) (f r3)
    omap f (Mslti r1 r2 i) = Mori (f r1) (f r2) i
    omap f (Mmult r1 r2) = Mmult (f r1) (f r2)
    omap f (Msw r1 i r2) = Msw (f r1) i (f r2)
    omap f (Mlw r1 i r2) = Mlw (f r1) i (f r2)
    omap _ (Mcomment s) = Mcomment s

    omap _ Msyscall = Msyscall

traverseMInstReg :: Applicative f => (MReg -> f MReg) -> MInst -> f MInst
traverseMInstReg f (Mli reg i) = liftA2 Mli (f reg) (pure i)
traverseMInstReg f (Mmflo reg) = Mmflo <$> (f reg)
traverseMInstReg f (Madd r1 r2 r3) = Madd <$> f r1 <*> f r2 <*> f r3
traverseMInstReg f (Maddi r1 r2 i) = Maddi <$> f r1 <*> f r2 <*> pure i
traverseMInstReg f (Mori r1 r2 i) = Mori <$> f r1 <*> f r2 <*> pure i
traverseMInstReg f (Mslt r1 r2 r3) = Mslt <$> f r1 <*> f r2 <*> f r3
traverseMInstReg f (Mslti r1 r2 i) = Mslti <$> f r1 <*> f r2 <*> pure i
traverseMInstReg f (Mmult r1 r2) = Mmult <$> f r1 <*> f r2
traverseMInstReg f (Msw r1 i r2) = Msw <$> f r1 <*> pure i <*> f r2
traverseMInstReg f (Mlw r1 i r2) = Mlw <$> f r1 <*> pure i <*> f r2
traverseMInstReg _ (Mcomment s) = pure (Mcomment s)
traverseMInstReg f Msyscall = pure Msyscall

mapMInstReg :: (MReg -> MReg) -> MInst -> MInst
mapMInstReg f inst = runIdentity $ traverseMInstReg (Identity . f) inst

-- | Collect a monoidal value from MReg over an MInst
foldMapMInstReg :: Monoid m => (MReg -> m) -> MInst -> m
foldMapMInstReg f inst = execState final Monoid.mempty where
  -- go :: MReg -> State m MReg
  go r = do 
          modify (\m -> m Monoid.<> f r)
          return r

  -- final :: State m Inst
  final = (traverseMInstReg go inst)


foldlMInstReg :: (seed -> MReg -> seed) -> seed -> MInst -> seed
foldlMInstReg f seed inst = execState final seed where
      -- go :: Reg -> State seed Reg
      go r = do 
              modify (\seed -> f seed r)
              return r

      -- final :: State m MInst
      final = traverseMInstReg go inst 

-- | Get the list of MRegs in a MInst
getMInstRegs :: MInst -> [MReg]
getMInstRegs = foldMapMInstReg (\r -> [r])

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
    pretty (Mmult a b) = pretty "mult" <+> pretty a <+> pretty b
    -- | Msw $src 20($s0)
    pretty (Msw a i b) = pretty "sw" <+> pretty a <+> pretty i PP.<> parens (pretty b)
    -- | Msw $dest 20($s0)
    pretty (Mlw a i b) = pretty "lw" <+> pretty a <+> pretty i PP.<> parens (pretty b)
    pretty (Mcomment s) = pretty "#" <+> pretty s
    pretty (Msyscall) = pretty "syscall"

data MTerminatorInst =
    Mexit | 
    Mj MBBLabel |
    Mbeqz MReg  MBBLabel |
    Mbgtz MReg MBBLabel deriving (Eq, Ord)

instance Pretty MTerminatorInst where
    pretty (Mexit) = pretty "# <exit>"
    pretty (Mj dest) = pretty "j" <+> pretty dest
    pretty (Mbeqz cond dest) = pretty "beqz" <+> pretty cond <+> pretty dest
    pretty (Mbgtz cond dest) = pretty "bgtz" <+> pretty cond <+> pretty dest

traverseMTerminatorInstReg :: Applicative f => (MReg -> f MReg) -> 
    MTerminatorInst -> f MTerminatorInst
traverseMTerminatorInstReg f Mexit = pure Mexit
traverseMTerminatorInstReg f (Mj lbl) = pure (Mj lbl)
traverseMTerminatorInstReg f (Mbeqz reg lbl) = Mbeqz <$> f reg <*> pure lbl
traverseMTerminatorInstReg f (Mbgtz reg lbl) = Mbgtz <$> f reg <*> pure lbl

mapMTerminatorInstReg :: (MReg -> MReg) -> MTerminatorInst -> MTerminatorInst
mapMTerminatorInstReg f t = 
    runIdentity $ traverseMTerminatorInstReg (Identity . f) t


type MBBLabel = Label MBB
type MBB = BasicBlock MInst [MTerminatorInst]
type MProgram = Program MInst [MTerminatorInst]


type MLiveRangeBB =  BasicBlock (Int, MInst) (Int, MTerminatorInst)
  
-- | Get the possible successor this terminator instruction will lead to.
getTerminatorInstSuccessor :: MTerminatorInst -> Maybe MBBLabel
getTerminatorInstSuccessor (Mexit) = Nothing
getTerminatorInstSuccessor (Mj lbl) = Just lbl
getTerminatorInstSuccessor (Mbgtz _ lbl) = Just lbl
getTerminatorInstSuccessor (Mbeqz _ lbl) = Just lbl

-- | Get the successors of this basic block
getMBBSuccessors :: MBB -> [MBBLabel]
getMBBSuccessors bb = bbRetInst bb >>= maybeToList . getTerminatorInstSuccessor


type MCFG = Graph MBBLabel
-- | Make a control flow graph
mkMCFG :: M.OrderedMap MBBLabel MBB -> MCFG
mkMCFG bbMap = Graph (M.foldMapWithKey makeEdges bbMap)  where
    makeEdges :: MBBLabel -> MBB -> [(MBBLabel, MBBLabel)]
    makeEdges bbid bb = map (\succ -> (bbid, succ)) (getMBBSuccessors bb)


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
