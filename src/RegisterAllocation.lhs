\begin{code}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RegisterAllocation(RegisterID,
NRegisters,
registerAllocate, LiveRange, makeLiveRange) where

import IR
import Graph

type RegisterID = Int
type NRegisters = Int

data LiveRange = LiveRange (Int, Int)


-- | Construct a live range from a begin and end time stamp
makeLiveRange :: Int -> Int -> LiveRange
makeLiveRange begin end = 
    if end < begin
    then error $ "need end" <+> 
                 braces (pretty end) <+>
                 pretty "> begin" <+>
                 braces (pretty begin) <+> 
                 pretty "for live range."

-- | Return if two live ranges overlap
doesLiveRangeOverlap :: LiveRange -> LiveRange -> Bool
doesLiveRangeOverlap (l1b, l1e) (l2b, l2e) =  (l1e > l2b) || (l1b < l2e)

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
