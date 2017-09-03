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
import Data.Bifunctor

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


\end{code}

