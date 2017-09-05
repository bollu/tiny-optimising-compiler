\begin{code}
{-# LANGUAGE ViewPatterns #-}

module TransformRegisterAllocate where
import qualified OrderedMap as M
import TransformMem2Reg (mkCFG, CFG)
import Control.Monad.State.Strict
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

nRegisters :: Int
nRegisters = 8

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
    assignRealReg (MRegVirtual (Label name)) = 
        case name `M.lookup` regmap of
            Just (Just rnum) -> mkTemporaryReg (rnum - 1) -- | -1 because colors are [1..n]
            Just (Nothing) -> error . docToString $ pretty "register needs to be spilled, unimplemented:" <+> pretty name
            Nothing -> error . docToString $ pretty "register not assigned a color at all: " <+> pretty name
    assignRealReg r = r


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
    indent 4 . pretty . colorRegisters $ mprogram]) (assignPhysicalRegisters coloring mprogram) where
        coloring = colorRegisters mprogram
\end{code}
