<h1> Tiny optimising compiler </h1>

Welcome to the tutorial series that teaches how to write a tiny optimising
compiler in haskell!



Start from:

1. [The source language.](language.html)

2. [The parser for the language.](parser.html)

3. [The internal representation.](ir.html)

4. [The `mem2reg` transform that lands us into `SSA`.](transformmem2reg.html)

5. [The `constant folding` transform that exploits `SSA` to "fold away" expressions
    which can be evaluated at compile time.](transformconstantfolding.html)

6. [The `register allocation` transform which allocates physical registers to
    the infinite virtual registers of our SSA form.](transformregisterallocate.html)

7. [The MIPS assembly specification](mipsasm.html), and the associated interpreter
   which uses [SPIM](mipsinterpreter.hs)

7. [The `mipsasm` code generation pass which generates MIPS assembly from our IR.](transformirtomips.html)
<h2> Background </h2>

I've wanted to write this for a while: a tiny *optimising* compiler for
a small imperative ish language.

I want to show off modern compiler ideas, such as:

- SSA.
- optimisations enabled by SSA.
- Scalar evolution.
- Polyhedral compilation

I currently have a parser for the source language, conversion to IR, then
to SSA, and a semi-broken MIPS backend.

<h4> Goals </h4>
- Be readable code.
- Be literate code (preferably).
- Show off real world optimisations.

<h4> Non goals </h4>
Shows the correct way of doing a lot of things, in the sense of "engineering". I
might pick the slower algorithm to compute a dominator tree, because I wish to
emphasize the _idea_ of the dominator tree. When a trade off is presented
between simplicity and performance, I will pick simplicity.


<h4> Timeline </h4>

- `[x]` Parse
- `[x]` Generate non-SSA IR
- `[x]` Convert non-SSA to SSA (`Mem2Reg` is the pass where this happens.)
- `[x]` generate MIPS assembly from SSA IR (half-done)
- `[ ]` (Optional) generate LLVM for SSA IR (Can be pulled from [simplexhc](http://github.com/bollu/simplexhc))

At this point, we have a "functioning" compiler. Now, we can extend the 
compiler or the language. I want to show off optimisations, so I will spend
more time implementing optimisations

- `[ ]` Loop detection.
- `[ ]` Scalar evolution.
- `[ ]` Global value numbering.
- `[ ]` Dead code elimination.
- `[ ]` Loop unrolling.
- `[ ]` invariant load hoisting.

Note that we do not yet have functions in the language! let's add that.

- `[ ]` extend language with functions.
- `[ ]` generate MIPS for functions.
- `[ ]` Inlining.


If we get here, we can then add polyhedral abilities to the compiler. For
this though, we would need to integrate with `isl`. **Someone** will need to write
haskell bindings `:)`.


