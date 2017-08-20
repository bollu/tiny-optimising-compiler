\begin{code}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RegisterAllocation(RegisterID,
NRegisters,
registerAllocate) where

import IR
import Graph

type RegisterID = Int
type NRegisters = Int

-- we are assuming that all our registers are 32 bit int registers
data RegisterAllocatorInput = RegisterAllocatorInput {
    totalIntRegisters :: Int
}

data RegisterAllocatorContext = RegisterAllocatorContext {
    ctxIrProgram :: IRProgram,
    ctxTotalIntRegisters :: Int,
    interferenceGraph :: Graph RegisterID
}

-- | HACK: for now, just make a complete graph to ensure full 
makeInterferenceGraph :: IRProgram -> Graph Label
makeInterferenceGraph = 

-- | Register allocate to the program
registerAllocate :: IRProgram -- ^ Program
                   -> NRegisters -- ^ Number of integer registers 
                   -> M.OrderedMap Label RegisterID -- ^ Map from label to register ID
registerAllocate nregisters program = (makeInterferenceGraph program) nregisters


\end{code}
