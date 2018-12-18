<h1> SCEV, or, how do we analyze loops?  </h1>

<h2> Equivalent LLVM passes </h2>

- [SCEV](http://llvm.org/doxygen/classllvm_1_1ScalarEvolution.html)


<h2> Introduction </h2>

SCEV is an analysis which allows us to understand recurrences across loops.

<h2> References </h2>

http://www.csd.uwo.ca/~moreno/CS447/Lectures/CodeOptimization.html/node6.html
http://web.cs.wpi.edu/~kal/PLT/PLT8.6.4.html

\begin{code}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module SCEV(analyzeSCEV, detectLoops) where

import IR
import BaseIR
import Data.Tree
import qualified Data.Set as S
import qualified OrderedMap as M
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils
import Control.Monad.Reader
import Data.Traversable
import qualified Data.Monoid as Monoid
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Control.Monad.State.Strict
import TransformMem2Reg
import Graph

data Loop = Loop {
    loopHeader :: IRBBId,
    loopBackEdges :: [(IRBBId, IRBBId)],
    loopLatches :: [IRBBId]
} 

instance Pretty Loop where
  pretty Loop{..} = 
      vsep [pheader, nest 4 platch, nest 4 pbackedges] where
      pheader = (pretty "header:") <+> (pretty loopHeader)
      platch =  vcat [pretty  "latches:",
                      nest 4 (vcat (fmap (pretty) loopLatches))]
      pbackedges = vcat [pretty "backedges:",
                         nest 4 $ vcat (fmap pretty loopBackEdges)]


-- | Returns if the given edge is a back-edge
-- | An edge (Start -> End) is a back edge if End dominates Start
-- | Perform this operation by checking if End belongs to Start's Domset.
isBackEdge :: BBIdToDomSet -> (IRBBId, IRBBId)  -> Bool
isBackEdge bbIdToDomSet (start, end) = end  `S.member` (bbIdToDomSet M.! start)



_detectLoopsRec :: M.OrderedMap IRBBId IRBB  -- ^ Basic Blocks in program
                  -> BBIdToDomSet -- ^ Mapping from basic blocks to nodes
                                  --   that dominate it
                  -> DomTree  -- ^ Domtree of program
                  -> CFG -- ^ CFG of program
                  -> IRBBId  -- ^Current Basic block being inspected
                  -> [Loop] -- ^ List of loops
_detectLoopsRec bbmap bbIdToDomSet domtree cfg curbbid = 
    curloop ++ (domtreechildren  >>= _detectLoopsRec bbmap bbIdToDomSet domtree cfg)
    where
    domtreechildren :: [IRBBId]
    domtreechildren = getImmediateChildren domtree curbbid

    -- | next nodes in the CFG from the current node
    cfgnext :: [(IRBBId, IRBBId)]
    cfgnext = getEdgesFromSource cfg curbbid 

    -- | backedges from the CFG
    backedges :: [(IRBBId, IRBBId)]
    backedges = filter (isBackEdge bbIdToDomSet) cfgnext

    -- | current loop if it exists
    curloop :: [Loop]
    curloop = if null backedges 
              then []
              else [Loop {
                loopHeader=curbbid,
                loopLatches= map fst backedges,
                loopBackEdges=backedges
              }]



detectLoops :: IRProgram -> [Loop]
detectLoops program@Program{programBBMap=bbmap,
                    programEntryBBId=entrybbid} = 
                    _detectLoopsRec bbmap bbIdToDomSet domtree cfg entrybbid where
    bbIdToDomSet :: BBIdToDomSet
    bbIdToDomSet = constructBBDominators program

    domtree :: DomTree
    domtree = constructDominatorTree bbIdToDomSet entrybbid

    cfg :: CFG
    cfg = mkCFG bbmap
 

-- | Chain of recurrences.
data SCEV = SCEV

type SCEVMap = M.OrderedMap (Label Inst) SCEV
 

analyzeSCEV :: IRProgram -> SCEVMap
analyzeSCEV = undefined
\end{code}
