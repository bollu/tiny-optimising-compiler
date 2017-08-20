<h1> Transform Pass: Constant Folding </h1>

In this pass, we remove all instructions we can evaluate at compile-time.
This includes arithmetic and boolean operators.

The idea is really simple: scan basic blocks, and if an instruction can be
immediately evaluated, do so.

Ideally, we do this in the top-down inorder of the dom tree, so that we 
will *always* have folded away possible values before we reach a node.

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

runTillStable :: Eq a => (a -> a) -> a -> a
runTillStable f a = let a' = f a in
    if a' == a
    then a'
    else f a'

transformConstantFold :: IRProgram -> IRProgram
transformConstantFold = runTillStable (dceProgram . foldProgram)  where

    -- | Collection of instruction names and values
    foldableInsts :: IRProgram -> [(Label Inst, Value)]
    foldableInsts p = foldMapIRProgramBBs (foldMapBB (collectFoldableInsts) (const mempty)) p

    -- | Program after constant folding
    foldProgram :: IRProgram -> IRProgram
    foldProgram program = foldl (\p (name, v) -> replaceUsesOfInst name v p) program (foldableInsts program)

    -- | program after dead code elimination
    dceProgram :: IRProgram -> IRProgram
    dceProgram program = 
        foldl (\p name -> mapIRProgramBBs (removeInstFromBB name) p) program (map fst (foldableInsts program))

\end{code}
