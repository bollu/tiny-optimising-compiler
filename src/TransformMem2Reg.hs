module TransformMem2Reg where
import IR
import Data.Tree

-- a dominator tree is a tree of basic blocks
newtype DominatorTree  = Tree BasicBlock

domtree :: IRProgram -> DominatorTree


mem2reg :: IRProgram -> IRProgram
