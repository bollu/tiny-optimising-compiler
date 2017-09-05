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

bumpCounter :: a -> State Int (Int, a)
bumpCounter a = do
                  count <- get
                  modify (+ 1)
                  return (count, a)

-- | TODO: we coalesce both real and virtual registers here, so I need to use
-- | String as the key. Find some way to keep type safety.
data LiveRangeBuilderContext = LiveRangeBuilderContext {
    -- | First use of register
    ctxBegin :: M.OrderedMap String Int,
    -- | Final use of register
    ctxEnd :: M.OrderedMap String Int
}

data LiveRange = LiveRange { lrName :: String, lrBegin :: Int, lrEnd :: Int }

-- | Two live ranges are equal when their intervals overlaps
instance Eq LiveRange where
    (LiveRange _ b1 e1) == (LiveRange _ b2 e2) = b1 == b2 && e1 == e2

-- | If L1 is contained in L2, then L1 < L2.
instance Ord LiveRange where
    l1@(LiveRange _ b1 e1) <= l2@(LiveRange _ b2 e2) = 
        l1 == l2 || (b1 >= b2 && e1 <= e2)

liveRangeIntersects :: LiveRange -> LiveRange -> Bool
liveRangeIntersects l1@(LiveRange _ b1 e1) l2@(LiveRange _ b2 e2) = 
    l1 <= l2 || e1 >= b2 && b1 <= e2 || 
    l2 <= l1 || e2 >= b1 && b2 <= e1 

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


-- | Program with each instruction timestamped
type MProgramTimestamped = Program (Int, MInst) [(Int, MTerminatorInst)]

timestampProgram :: MProgram -> MProgramTimestamped
timestampProgram p = evalState (traverseProgramBBs (traverseBB bumpCounter (\ris -> for ris bumpCounter)) p) 0

mkInterferenceGraph :: MProgram -> Graph (Label MInst)
mkInterferenceGraph = undefined

transformRegisterAllocate :: MProgram -> MProgram
transformRegisterAllocate mprogram = trace (docToString $ 
    vcat [pretty "timestamped program:",
    indent 4 $ pretty (timestampProgram mprogram),
    pretty "live range info: ",
    indent 4. pretty . mkLiveRangeBuilderContext . timestampProgram $ mprogram]) mprogram
\end{code}
