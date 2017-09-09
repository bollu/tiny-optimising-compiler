\begin{code}
{-# LANGUAGE ViewPatterns #-}

module TransformRegisterAllocate where
import qualified OrderedMap as M
import TransformMem2Reg (mkCFG, CFG)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Traversable
import Data.Foldable
import Control.Applicative
import qualified Data.List.NonEmpty as NE
import Data.List(sortBy)
import IR
import Graph
import BaseIR
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils
import Debug.Trace
import Data.Maybe (isJust)
import MIPSAsm
import qualified Data.Set as S

nRegisters :: Int
nRegisters = 1

bumpCounter :: a -> State Int (Int, a)
bumpCounter a = do
                  count <- get
                  modify (+ 1)
                  return (count, a)

-- | the type of an interference graph.
type InterferenceGraph = Graph String

-- | TODO: we coalesce both real and virtual registers here, so I need to use
-- | String as the key. Find some way to keep type safety.
data LiveRangeBuilderContext = LiveRangeBuilderContext {
    -- | First use of register
    ctxBegin :: M.OrderedMap String Int,
    -- | Final use of register
    ctxEnd :: M.OrderedMap String Int
}

data LiveRange = LiveRange { lrName :: String, lrBegin :: Int, lrEnd :: Int } deriving(Eq)


-- | If L1 is contained in L2, then L1 < L2.
instance Ord LiveRange where
    l1@(LiveRange _ b1 e1) <= l2@(LiveRange _ b2 e2) = 
        l1 == l2 || (b1 >= b2 && e1 <= e2)

-- | Arrange live ranges as (left, right) where
-- | begintime of left <= begin time of right
arrangeLiveRangePair :: LiveRange ->  LiveRange -> (LiveRange, LiveRange)
arrangeLiveRangePair l1@(LiveRange _ b1 _) l2@(LiveRange _ b2 _) = 
    if b1 <= b2 then (l1, l2) else (l2, l1)

-- | Compute the length of a live range
liveRangeLength :: LiveRange -> Int
liveRangeLength (LiveRange _ b e) = e - b + 1

-- | Check if the live ranges intersect.
-- | If the length of the hull is less than or equal to the sum of
-- | lengths of live ranges, then they do not intersect.
liveRangeIntersects :: LiveRange -> LiveRange -> Bool
liveRangeIntersects l1 l2 = let
    (LiveRange _ b _, LiveRange _ _ e) = arrangeLiveRangePair l1 l2
    in
        e - b + 1 <= liveRangeLength l1 + liveRangeLength l2

-- | Make an interference graph from the given live ranges.
mkLiveRangeInterferenceGraph :: [LiveRange] -> InterferenceGraph
mkLiveRangeInterferenceGraph lrs = Graph $ do
    l1 <- lrs
    l2 <- lrs
    guard $ l1 /= l2
    guard $ liveRangeIntersects l1 l2
    [(lrName l1, lrName l2), (lrName l2, lrName l1)]

-- | Arrange by the begin time
arrangeByStart :: [LiveRange] -> [LiveRange]
arrangeByStart = sortBy (\(lrBegin -> b1) (lrBegin -> b2) -> b1 `compare` b2)

-- | Arrange by the end time
arrangeByEnd :: [LiveRange] -> [LiveRange]
arrangeByEnd = sortBy (\(lrEnd -> e1) (lrEnd -> e2) -> e1 `compare` e2)


    
instance Pretty LiveRange where
    pretty (LiveRange name b e) = pretty name <+> pretty ":" <+> parens (pretty b <+> pretty "to" <+> pretty e)

instance Pretty LiveRangeBuilderContext where
    pretty ctx = 
        if null ls
        then pretty "EMPTY"
        else vcat $ map pretty  ls
        where
        ls = mkLiveRangesFromContext ctx

defaultLiveRangeBuilderContext :: LiveRangeBuilderContext
defaultLiveRangeBuilderContext = LiveRangeBuilderContext mempty mempty

-- | Record the use of a register
-- | If we have already have this in "begin", then keep it the same,
-- | and edit the "end". Otherwise, add the register to both "begin" and "end"
recordRegisterUse :: Int -> MReg -> State LiveRangeBuilderContext MReg
recordRegisterUse  pos reg@(MRegVirtual (Label name)) = do
    hasbegin <- gets (\(LiveRangeBuilderContext{ctxBegin=begin}) -> 
                      isJust (M.lookup name begin))
    if not hasbegin then
        modify (\(ctx@LiveRangeBuilderContext{ctxBegin=begin}) ->
                ctx {
                    ctxBegin=M.insert name pos begin
                })
    else
        return ()

    modify (\ctx@LiveRangeBuilderContext{ctxEnd=end} ->
                ctx {
                    ctxEnd=M.insert name pos end
                })
    return reg

-- TODO: we need to implement this for real registers as well (interference)
recordRegisterUse _ reg = return reg 

-- | Construct a LiveRangeBuilderContext from a timestamped program.
-- | This essentially corresponds to finding time of creation and time of
-- | last use for every register.
-- | TODO: rewrite with foldr, only sensible way to write this.
mkLiveRangeBuilderContext :: MProgramTimestamped -> LiveRangeBuilderContext
mkLiveRangeBuilderContext progTimestamped = 
    execState s defaultLiveRangeBuilderContext where
        s :: State LiveRangeBuilderContext (Program MInst [MTerminatorInst])
        s = traverseProgramBBs (traverseBB
                                (\(i, reg) -> traverseMInstReg (recordRegisterUse i) reg)
                                (\ris -> for ris (\(i, ri) -> traverseMTerminatorInstReg (recordRegisterUse i) ri))) progTimestamped


mkLiveRangesFromContext :: LiveRangeBuilderContext -> [LiveRange]
mkLiveRangesFromContext (LiveRangeBuilderContext begin end) = lrs
    where
    ks :: [String]
    ks = M.keys begin

    lrs :: [LiveRange]
    lrs = map (\k -> LiveRange k (begin M.! k) (end M.! k)) ks


-- | Assign physical registers to all virtual registers
-- | Once this function is called, all variables are assigned physical registers
-- | TODO: implement spilling.
assignPhysicalRegisters :: M.OrderedMap String (Maybe Int) -> MProgram -> MProgram
assignPhysicalRegisters regmap p = 
    mapProgramBBs 
        (mapBB (mapMInstReg assignRealReg)
                (map (mapMTerminatorInstReg assignRealReg))) p where
    assignRealReg :: MReg -> MReg
    assignRealReg vreg@(MRegVirtual (Label name)) = 
        case name `M.lookup` regmap of
            Just (Just rnum) -> mkTemporaryReg (rnum - 1) -- | -1 because colors are [1..n]
            -- Keep registers to be spilled
            Just (Nothing) -> vreg -- error . docToString $ pretty "register needs to be spilled, unimplemented:" <+> pretty name
            Nothing -> error . docToString $ pretty "register not assigned a color at all: " <+> pretty name
    assignRealReg r = r


-- | The worker registers that are available. We need to make sure that
-- | there are at least 2 worker registers.
spillWorkRegs :: WorkerRegs 
spillWorkRegs = WorkerRegs . S.fromList $ map mkTemporaryReg [nRegisters..7]

newtype StackOffset = StackOffset { unStackOffset ::  Int }

instance Pretty StackOffset where
    pretty (StackOffset loc) = pretty "stackoffset" PP.<> braces (pretty loc)


-- | A structure to represent instructions used to spill registers.
-- | Has a convenient monoidal structure which we exploit.
data SpillInsts = SpillInsts {
    spillInstsPre :: [MInst],
    spillInstsPost :: [MInst]
}

instance Monoid SpillInsts where
    mempty = SpillInsts mempty mempty
    (SpillInsts pre1 post1) `mappend` (SpillInsts pre2 post2) = 
        SpillInsts (pre2 ++ pre1) (post1 ++ post2)



-- | Make a SpillInsts structure that shows how to setup a worker register
-- | And how to reload the old state.
mkSpillInsts :: (MReg, StackOffset) -- ^ Real Register to be used for operations with stack offset
    -> (MReg, StackOffset) -- ^ Register to be spilled with stack offset
    -> SpillInsts
mkSpillInsts (real, (StackOffset realso)) (virtual, (StackOffset virtualso))
    = SpillInsts pre post where
        pre = [Msw real realso regsp,
               Mlw real virtualso regsp]

        post = [Msw real virtualso regsp,
                Mlw real realso regsp]

-- | List of worker registers that are in use. Note that this is dynamic,
-- | because trying to "legalise" an instruction might grab multiple
-- | worker registers
data WorkerRegs = WorkerRegs { getWorkerRegs :: S.Set MReg }

type SpillM a = StateT WorkerRegs (ReaderT SpillContext (Writer SpillInsts)) a

-- | Run a SpillM, given a seed spilling context and a collection of
-- | available worker registers
runSpillM :: WorkerRegs -> SpillContext -> SpillM a -> (a, SpillInsts)
runSpillM workers ctx  spillm = runWriter $ runReaderT (evalStateT spillm workers) ctx
saveWorkerRegs :: WorkerRegs -> SpillM a -> SpillM a
saveWorkerRegs workers sa = do
    -- start with workers
    put workers
    a <- sa
    -- restore workers
    put workers
    return a

-- | Get a worker register for a task. Note that this does not
-- | release the worker register.
-- | If no worker registers are available, then error out.
getWorkerReg :: SpillM MReg
getWorkerReg = do
    workers <- gets getWorkerRegs
    if S.null workers
    then error "there are no worker registers available."
    else do
        let cur = S.elemAt 0 workers
        put $ WorkerRegs (S.deleteAt 0 workers)
        return cur

-- | Context for spilling
data SpillContext = SpillContext {
    spillCtxUncoloredRegs :: S.Set (Label MReg),
    spillCtxStackOffsets :: M.OrderedMap String StackOffset
}

mkSpillContext :: [Label MReg] -- ^ Registers to spill
    -> WorkerRegs -- ^ Worker registers
    -> SpillContext
mkSpillContext tospill (WorkerRegs workers) =
    SpillContext (S.fromList tospill) offsets where
        offsets :: M.OrderedMap String StackOffset
        offsets = M.fromList (zip allregs (map StackOffset [0,-4..]))

        allregs :: [String]
        allregs = map unLabel tospill ++ map regToString (S.toList workers)
getSpillContextRegOffset :: String -> SpillM StackOffset 
getSpillContextRegOffset name = asks (\ctx -> (spillCtxStackOffsets ctx) M.! name)

-- | If a register is uncolored, provide the instructions needed to perform
-- | A correct Spill/Unspill
spillRegister :: MReg -- ^ Current register
                -> SpillM MReg
spillRegister cur@(MRegVirtual lbl) = do
    isUncolored <- asks (\ctx -> lbl `S.member` (spillCtxUncoloredRegs ctx))
    if not isUncolored
    then return cur
    else do
        -- | allocate a real register and hold on to it.
        real <- getWorkerReg
        realoffset <- getSpillContextRegOffset $ regToString real
        curoffset <- getSpillContextRegOffset $ regToString cur
        tell $ mkSpillInsts (real, realoffset) (cur, curoffset)
        return real
spillRegister cur = return cur


-- | Spill the registers in a given instruction, to create a sequence of
-- | instructions that spill and restore if needed
spillInstRegs :: WorkerRegs -> SpillContext -> MInst -> [MInst]
spillInstRegs workers ctx inst = preInsts ++ [inst'] ++ postInsts where
    spiller :: SpillM MInst
    spiller = ((saveWorkerRegs workers) (traverseMInstReg spillRegister inst))

    (inst', SpillInsts preInsts postInsts) = runSpillM workers ctx spiller


-- | The SpillM which can spill  the registers in an terminator instruction
spillerTerminatorInstRegs_ :: WorkerRegs -> MTerminatorInst -> SpillM MTerminatorInst
spillerTerminatorInstRegs_ workers retinst = 
    ((saveWorkerRegs workers) (traverseMTerminatorInstReg spillRegister retinst))

-- | Spill registers in a given terminator
spillTerminatorInstRegs :: MCFG -- ^ control flow graph
    -> MBBLabel -- ^ current BB id
    -> MProgram -- ^ program
    -> MProgram
spillTerminatorInstRegs cfg curbbid p = p

-- | The entry point to spilling code
spillVirtualRegisters :: [Label MReg] -- ^ Registers to spill
    -> MProgram -> MProgram
spillVirtualRegisters tospill p = spillTerminators_ . spillInsts_ $ p where
    spillTerminators_ :: MProgram -> MProgram
    spillTerminators_ = id

    spillInsts_ :: MProgram -> MProgram
    spillInsts_ = mapProgramBBs (mapBBInstLocus (spillInstRegs spillWorkRegs spillctx))

    spillctx :: SpillContext
    spillctx = mkSpillContext tospill spillWorkRegs

    cfg :: MCFG
    cfg = mkMCFG (programBBMap p)

-- | Construct an interference graph of the given program.
mkInterferenceGraph :: MProgram -> InterferenceGraph
mkInterferenceGraph = mkLiveRangeInterferenceGraph . mkLiveRangesFromContext . mkLiveRangeBuilderContext . timestampProgram


-- | Color the registers of a program.
-- | Returns a map from register name to the color.
-- | If a name does not exist on the map, then it must be spilled.
colorRegisters :: MProgram -> M.OrderedMap String (Maybe GraphColor)
colorRegisters = greedyColorGraph nRegisters . mkInterferenceGraph

-- | Program with each instruction timestamped
type MProgramTimestamped = Program (Int, MInst) [(Int, MTerminatorInst)]

timestampProgram :: MProgram -> MProgramTimestamped
timestampProgram p = evalState (traverseProgramBBs (traverseBB bumpCounter (\ris -> for ris bumpCounter)) p) 0


transformRegisterAllocate :: MProgram -> MProgram
transformRegisterAllocate mprogram = trace (docToString $ 
    vcat [pretty "timestamped program:",
    indent 4 $ pretty (timestampProgram mprogram),
    pretty "live range info: ",
    indent 4. pretty . mkLiveRangeBuilderContext . timestampProgram $ mprogram,
    pretty "interference graph:",
    indent 4. pretty . mkInterferenceGraph $ mprogram,
    pretty "coloring:",
    indent 4 . pretty . colorRegisters $ mprogram,
    pretty "physical regs assigned program: ",
    indent 4 . pretty $ physicalAssignedProgram,
    pretty "Spilled program: ",
    indent 4 . pretty $ spilledProgram]) spilledProgram where
        coloring = colorRegisters mprogram

        physicalAssignedProgram :: MProgram
        physicalAssignedProgram = (assignPhysicalRegisters coloring mprogram)

        spilledProgram :: MProgram
        spilledProgram = spillVirtualRegisters registersToSpill physicalAssignedProgram

        registersToSpill :: [Label MReg]
        registersToSpill = M.foldMapWithKey (\k mColor -> case mColor of 
                                                            Just _ -> []
                                                            Nothing -> [Label k]) coloring
\end{code}
