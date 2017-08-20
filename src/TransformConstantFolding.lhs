\begin{code}
{-# LANGUAGE ViewPatterns #-}

module TransformConstantFolding where
import qualified OrderedMap as M
import Control.Monad.State.Strict
import Data.Traversable
import Data.Foldable
import Control.Applicative
import qualified Data.List.NonEmpty as NE
import IR
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils

-- | Fold all possible arithmetic / boolean ops
tryFoldInst :: Inst -> Maybe Value
tryFoldInst (InstAdd (ValueConstInt i) (ValueConstInt j)) = 
    Just $ ValueConstInt (i + j)
tryFoldInst (InstMul (ValueConstInt i) (ValueConstInt j)) = 
    Just $ ValueConstInt (i * j)
tryFoldInst (InstL (ValueConstInt i) (ValueConstInt j)) = 
    Just $ if i < j then ValueConstInt 0 else ValueConstInt 1
tryFoldInst (InstAnd (ValueConstInt i) (ValueConstInt j)) = 
    Just $ ValueConstInt (i * j)
tryFoldInst i = Nothing

collectFoldableInsts :: Named Inst -> [(Label Inst, Value)]
collectFoldableInsts (Named name (tryFoldInst -> Just v)) = [(name, v)]
collectFoldableInsts _ = []


transformConstantFold :: IRProgram -> IRProgram
transformConstantFold program = dcedProgram where

    -- | Collection of instruction names and values
    foldableInsts :: [(Label Inst, Value)]
    foldableInsts = foldMapIRProgramBBs (foldMapBB (collectFoldableInsts) (const mempty)) program

    -- | Program after constant folding
    foldedProgram :: IRProgram
    foldedProgram = foldl (\p (name, v) -> replaceUsesOfInst name v p) program foldableInsts

    -- | program after dead code elimination
    dcedProgram :: IRProgram
    dcedProgram = foldl (\p name -> mapIRProgramBBs (removeInstFromBB name) p) foldedProgram (map fst foldableInsts)

\end{code}