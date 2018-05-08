<h1> SCEV, or, how do we analyze loops?  </h1>

<h2> Equivalent LLVM passes </h2>

- [SCEV](http://llvm.org/doxygen/classllvm_1_1ScalarEvolution.html)


<h2> Introduction </h2>

SCEV is an analysis which allows us to understand recurrences across loops.

\begin{code}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SCEV(analyzeSCEV) where

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
    loopPreheader :: IRBBId,
    loopLatches :: [IRBBId],
    loopInnerLoops :: [Loop]
}

_detectLoopsRec :: IRBBId  -- ^Current Basic block being inspected
                  -> M.OrderedMap IRBBId IRBB  -- ^ Basic Blocks in program
                  -> DomTree  -- ^ Domtree of program
                  -> [Loop] -- ^ List of loops
_detectLoopsRec curbbid bbmap domtree = []

detectLoops :: IRProgram -> [Loop]
detectLoops program@Program{programBBMap=bbmap,
                    programEntryBBId=entrybbid} = 
                    _detectLoopsRec entrybbid bbmap domtree where
    bbIdToDomSet :: BBIdToDomSet
    bbIdToDomSet = constructBBDominators program

    domtree :: DomTree
    domtree = constructDominatorTree bbIdToDomSet entrybbid
 

data SCEVType = Add | Mul

data SCEV = SCEV {
  scevType :: SCEVType,
  scevInit :: SCEV,
  scevRec :: SCEV
}

type SCEVMap = M.OrderedMap (Label Inst) (SCEV)
 

analyzeSCEV :: IRProgram -> SCEVMap
analyzeSCEV = undefined
\end{code}
