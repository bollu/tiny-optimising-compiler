# A simple end-to-end compiler for an imperative programming language

- `Program -> SSA -> LLVM`
- `Program -> SSA -> MIPS` (later on)


I've wanted to write this for a while: a tiny *optimising* compiler for
a small imperative ish language.

I want to show off modern compiler ideas, such as:

- SSA
- optimisations enabled by SSA
- Scalar evolution
- Polyhedral compilation

I currently have a parser and a non-ssa IR. I'm writing the dominator tree
construction + dominance frontier algorithm to convert to SSA.

At that point, we can generate LLVM with the phi nodes.

#### Goals
- Be readable code.
- Be literate code (preferably).
- Show off real world optimisations.

#### Non goals
Shows the correct way of doing a lot of things, in the sense of "engineering". I
might pick the slower algorithm to compute a dominator tree, because I wish to
emphasize the _idea_ of the dominator tree. When a trade off is presented
between simplicity and performance, I will pick simplicity.


#### Timeline

- [x] Parse
- [x] Generate non-SSA IR
- [x] Convert non-SSA to SSA (`Mem2Reg` is the pass where this happens.)
- [ ] generate LLVM for SSA IR (Can be pulled from [simplexhc](http://github.com/bollu/simplexhc))
- [ ] generate MIPS assembly

At this point, we have a "functioning" compiler. Now, we can extend the 
compiler or the language. I want to show off optimisations, so I will spend
more time implementing optimisations

- [ ] Global value numbering
- [ ] Dead code elimination
- [ ] Loop unrolling
- [ ] invariant load hoisting
- etc.

Note that we do not yet have functions in the language! let's add that.
- [ ] extend language with functions
- [ ] generate LLVM for functions
- [ ] generate MIPS for functions

Now, we can go to the fancy stuff: inter procedural optimisation, scalar
evolution, etc.

- [ ] implement scalar evolution
- [ ] implement SCEV

If we get here, we can then add polyhedral abilities to the compiler. For
this though, we would need to integrate with `isl`. **Someone** will need to write
haskell bindings `:)`.


