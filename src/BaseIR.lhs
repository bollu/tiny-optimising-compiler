<h1> BaseIR </h1>

This module contains the building blocks that are shared across the `IR` and
the `MIPSAsm` module. They both use ideas of `Program`, `BasicBlock`, etc, but 
with slightly different underlying types. Hence, we unify the common code here.
\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module BaseIR where

import qualified Control.Arrow as A
import Data.Text.Prettyprint.Doc as PP
import qualified OrderedMap as M
import Data.Functor.Identity
import Data.Traversable
import qualified Data.Monoid as M
import Control.Monad
import Data.Bifunctor
import PrettyUtils

-- | A label that uses the phantom @a as a type based discriminator
data Label a = Label { unLabel ::  String } deriving(Eq, Ord, Functor, Foldable, Traversable)
instance Pretty (Label a) where
  pretty (Label s) = pretty s


-- | Convert from one type of label to another label.
unsafeTransmuteLabel :: Label a -> Label b
unsafeTransmuteLabel (Label lbl) = Label lbl

-- | A basic block. Single-entry, multiple-exit.
-- | TODO: remove duplication of information about the bbLabel in both
-- | Program and BasicBlock.
data BasicBlock inst ret = BasicBlock {
  bbInsts :: [inst],
  bbRetInst :: ret ,
  bbLabel :: Label (BasicBlock inst ret)
}

deriving instance (Eq inst, Eq ret) =>  Eq (BasicBlock inst ret)

-- | Used to identify basic blocks
type BBId inst retinst = Label (BasicBlock inst retinst)

-- TODO: replace nest with indent
instance (Pretty inst, Pretty ret) => Pretty (BasicBlock inst ret)where
  pretty (BasicBlock insts ret label) =
    nest 4 (vsep ([pretty label <> pretty ":"] ++ body)) where
      body = map pretty insts ++ [pretty ret]


data Program inst ret = Program  {
  programBBMap :: M.OrderedMap (BBId inst ret) (BasicBlock inst ret),
  programEntryBBId :: (BBId inst ret)
}


deriving instance (Eq inst, Eq ret) =>  Eq (Program inst ret)

instance (Pretty inst, Pretty ret) => Pretty (Program inst ret) where
  pretty (Program bbmap entryId) =
    vsep $ [pretty "entry: " <+> pretty entryId, pretty "program: "] ++
                                            fmap pretty (M.elems bbmap)



-- | Run an effect at a particular basic block for a program
traverseProgramAt :: Applicative f => BBId inst ret 
  -> (BasicBlock inst ret -> f (BasicBlock inst ret))
  -> Program inst ret ->
  f (Program inst ret)
traverseProgramAt bbid f (Program bbmap entryId) =  Program <$> bbmap' <*> pure entryId
  where
    bbmap' = (\curbb' -> M.insert bbid curbb' bbmap) <$> (f curbb)
    curbb = case M.lookup bbid bbmap of
              Just bb -> bb
              Nothing -> error . docToString $ pretty "unable to find bbid in program: " <+> pretty bbid


mapProgramAt :: BBId inst ret -> (BasicBlock inst ret -> BasicBlock inst ret)
  -> Program inst ret -> Program inst ret
mapProgramAt bbid f p = runIdentity $ 
  traverseProgramAt bbid (Identity . f) p


-- | Map an effect over all the BBs of the Program
traverseProgramBBs :: Applicative f =>
  (BasicBlock inst ret -> f (BasicBlock inst' ret'))
  -> Program inst ret
  -> f (Program inst' ret')
traverseProgramBBs fbb (Program bbmap entrybbid) =
    (Program <$> bbmap' <*> pure (unsafeTransmuteLabel entrybbid)) where
        -- bbmap' :: M.OrderedMap (BBId inst' ret') (BasicBlock inst' ret')
        bbmap' = traverse fbb bbmapRekeyed

        -- bbmapRekeyed :: M.OrderedMap (BBId inst' ret') (BasicBlock inst ret)
        bbmapRekeyed = M.editKeys unsafeTransmuteLabel bbmap

-- | Map a pure effect over all BBs of the IRPRogram
mapProgramBBs :: (BasicBlock inst ret -> BasicBlock inst' ret')
  -> Program inst ret
  -> Program inst' ret'
mapProgramBBs fbb program = runIdentity $ traverseProgramBBs (Identity . fbb) program

-- | Run a monadic effect over the basic blocks throwing away the results
mapMProgramBBs_ :: Monad m => (BasicBlock inst ret -> m ()) -> Program inst ret -> m ()
mapMProgramBBs_ fbb (Program bbmap _) = forM_ bbmap fbb


-- | Collect results from basic blocks which can be monoidally smashed.
foldMapProgramBBs :: Monoid m =>
  (BasicBlock inst ret -> m)
  -> Program inst ret
  -> m
foldMapProgramBBs fbb program = foldMap fbb (programBBMap program)

-- | Filter instructions in a basic block.
filterBBInsts :: (inst -> Bool) -> BasicBlock inst ret -> BasicBlock inst ret
filterBBInsts pred (BasicBlock insts retinst lbl) =
        BasicBlock insts' retinst lbl
    where insts' = filter pred insts

-- | Filter instructions in a Program.
filterProgramInsts :: (inst -> Bool) -> Program inst ret -> Program inst ret
filterProgramInsts pred prog =
  mapProgramBBs (filterBBInsts pred) prog

-- | Run an effect on a basic block.
traverseBB :: Applicative f => (inst -> f inst')
         -> (ret -> f ret')
         -> BasicBlock inst ret
         -> f (BasicBlock inst' ret')
traverseBB finst fretinst (BasicBlock insts retinst lbl) =
    BasicBlock <$> insts' <*> retinst' <*> pure (unsafeTransmuteLabel lbl) where
        retinst' = fretinst retinst
        insts' = for insts finst

-- | Run an effect over a basic block throwing away the results
mapMBB_ :: Monad f => (inst -> f ()) -> (ret -> f ()) -> BasicBlock inst ret -> f ()
mapMBB_ finst fretinst (BasicBlock insts retinst lbl) = do
    for insts finst
    fretinst retinst


weaveEffect_ :: (Traversable f, Applicative f, Monad t, Traversable t) => (a -> f (t b))
  -> t a -> f (t b)
weaveEffect_ f as = join <$> intermediate -- f (t t b)
  where
    intermediate = for as f
    -- join :: t (t b) -> t b
    join ttb = ttb >>= (\tb -> tb)

-- | Run an effect on a basic block, while allowing to create a "locus" around
-- | an instruction. This can be used to delete instructions, or add a sequence
-- | of instructions for one original instruction.
traverseBBInstLocus :: (Applicative f, Traversable f) => 
  (inst -> f [inst'])
  -> BasicBlock inst ret 
  -> f (BasicBlock inst' ret)
traverseBBInstLocus finst (BasicBlock insts retinst lbl) =

    BasicBlock <$> insts'<*> pure retinst <*> pure (unsafeTransmuteLabel lbl) where
      insts' = weaveEffect_ finst insts


mapBBInstLocus :: (inst -> [inst']) -> BasicBlock inst ret -> BasicBlock inst' ret
mapBBInstLocus f bb = runIdentity $ traverseBBInstLocus (Identity . f) bb
-- | Fold from the first instruction to the last one, and then on the
-- | RetInst of a BB.
foldlBB :: collect
           -> (collect -> inst -> collect)
           -> (collect -> ret -> collect)
           -> BasicBlock inst ret
           -> collect
foldlBB seed finst fretinst (BasicBlock insts retinst lbl) =
    fretinst (foldl finst seed insts) retinst


-- | produce results on a BB and smash them together with  a monoid instance
foldMapBB :: Monoid m => (inst -> m)
    -> (ret -> m)
    -> BasicBlock inst ret
    ->  m
foldMapBB finst fretinst bb =
    foldlBB mempty (\c i -> c M.<>  finst i) (\c ri -> c M.<> fretinst ri) bb


-- | Map over the instructions and return values of a basic block
mapBB :: (inst -> inst')
          -> (ret -> ret')
          -> BasicBlock inst ret
          -> BasicBlock inst' ret'
mapBB finst fretinst bb =
    runIdentity $ traverseBB (Identity . finst) (Identity . fretinst) bb


-- | Insert instructions before the first instruction in a bb.
insertInstsBeginBB :: [inst] -> BasicBlock inst ret -> BasicBlock inst ret
insertInstsBeginBB pre (BasicBlock insts retinst lbl) =
  BasicBlock (pre++insts) retinst lbl

-- | Insert instructions at the end of the last instruction in a bb.
insertInstsEndBB :: [inst] -> BasicBlock inst ret -> BasicBlock inst ret
insertInstsEndBB post (BasicBlock insts retinst lbl) =
  BasicBlock (insts++post) retinst lbl


\end{code}

