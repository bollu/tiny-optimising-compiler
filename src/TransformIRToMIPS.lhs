<h1> Transform Pass: IR Canonicalization for MIPS </h1>


<h2> Introduction </h2>

<h4> In this pass, we rewrite binary instructions of the form: </h4>
- `<inst> <int> <ref>`
- `<inst> <ref> <int>`
to
```
<inst> <int> <ref>
```


<h4> We assume that constant folding has already taken place,
so we cannot have: </h4>
```
<inst> <int> <int>
```

<h4> We will leave: </h4>
- `<inst> <ref> <ref>`
as-is.


\begin{code}
{-# LANGUAGE ViewPatterns #-}

module TransformIRToMIPS where
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

tryRearrangeVals :: (Value, Value) -> (Value, Value)
tryRearrangeVals(v@(ValueConstInt _), w@(ValueConstInt _)) =
    error . docToString $ pretty "this pass assumes that constant folding has already been run."
tryRearrangeVals (v, w@(ValueConstInt _)) = (w, v)
tryRearrangeVals (v, w) = (v, w)

-- | Try to rearrange the instruction parameters
tryRearrangeInst :: Inst -> Inst
tryRearrangeInst (InstAdd v w) = uncurry InstAdd (tryRearrangeVals (v, w))
tryRearrangeInst (InstMul v w) = uncurry InstMul (tryRearrangeVals (v, w))
tryRearrangeInst (InstL v w) = uncurry InstL (tryRearrangeVals (v, w))
tryRearrangeInst (InstAnd v w) = uncurry InstAnd (tryRearrangeVals (v, w))
tryRearrangeInst i = i


transformCanonicalizeForMIPS :: IRProgram -> IRProgram
transformCanonicalizeForMIPS = mapProgramBBs (mapBB id id)

\end{code}

