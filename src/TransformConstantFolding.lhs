<h1> Transform Pass: Constant Folding </h1>

- [Equivalent LLVM pass](https://llvm.org/docs/Passes.html#constprop-simple-constant-propagation)

<h2> Introduction </h2>

In this pass, we remove all instructions we can evaluate at compile-time.
This includes arithmetic and boolean operators.

The idea is really simple: scan basic blocks, and if an instruction can be
immediately evaluated, do so.

Note that for this pass to be as easy as it is, **SSA is crucial**.

Consider this snippet of code:

```
define x;
assign x := 10;
assign x := x + 42;
assign x := x * 10
return x;
```

and the associated `load/store` based IR:

```
entry:  default.0
program:
default.0:
    x := alloc
    _ := store 10# in %x
    x.load := load %x
    tmp.0 := add %x.load 42#
    _.1 := store %tmp.0 in %x
    x.load.1 := load %x
    tmp.1 := mul %x.load.1 10#
    _.2 := store %tmp.1 in %x
    TERMINAL
```

We cannot simply replace `x` with `10` due to the mutation happening on x!

Now, consider the SSA form of the same computation:

```
entry:  default.0
program:
default.0:
    tmp.0 := add 10# 42#
    tmp.1 := mul %tmp.0 10#
    TERMINAL
```

Due to the *immutable* nature of SSA, we are guaranteed that we can replace all
occurences of a variable with it's RHS, and the semantics of the program will
remain the same! (AKA [equational reasoning](https://wiki.haskell.org/Equational_reasoning_examples)).

This is enormously powerful because it allows to replace values with wild abandon `:)`.

<h2> Key Takeaway of this pass </h2>

- SSA, due to immutability enables equational reasoning.
- This allows us to perform transformations such as
  constant folding very easily.



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
import BaseIR
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

-- | Fold all possible arithmetic / boolean ops
tryFoldInst :: Inst -> Maybe Value
tryFoldInst (InstAdd (ValueConstInt i) (ValueConstInt j)) = 
    Just $ ValueConstInt (i + j)
tryFoldInst (InstMul (ValueConstInt i) (ValueConstInt j)) = 
    Just $ ValueConstInt (i * j)
tryFoldInst (InstL (ValueConstInt i) (ValueConstInt j)) = 
    Just $ ValueConstInt $ boolToInt (i < j)

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
    foldableInsts p = foldMapProgramBBs (foldMapBB (collectFoldableInsts) (const mempty)) p

    -- | Program after constant folding
    foldProgram :: IRProgram -> IRProgram
    foldProgram program = foldl (\p (name, v) -> replaceUsesOfInst name v p) program (foldableInsts program)

    -- | program after dead code elimination
    dceProgram :: IRProgram -> IRProgram
    dceProgram program =
        foldl (\p name -> filterProgramInsts (not . hasName name) p) program (map fst (foldableInsts program))

\end{code}
