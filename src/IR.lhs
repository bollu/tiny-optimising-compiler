<h1>  Internal Representation </h1>

In this module, we define the LLVM-like IR that we compile our
source code to.

\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IR where
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils
import qualified Language as L
import qualified Data.List.NonEmpty as NE
import qualified OrderedMap as M
import Data.Functor.Identity
import qualified Data.Monoid as Monoid
import BaseIR
import Data.Traversable(for)
import Control.Applicative(liftA2)
import Control.Monad.State.Strict(State, execState, modify)

type IRBB = BasicBlock (Named Inst) RetInst
type IRBBId = BBId (Named Inst) (RetInst)

-- | Default basic block.
defaultIRBB :: IRBB
defaultIRBB = BasicBlock [] (RetInstTerminal) (Label "undefined")

-- | Given an IRBB, return a list of Phi nodes.
getIRBBPhis :: IRBB -> [Named Inst]
getIRBBPhis bb = bbInsts $
  filterBBInsts (\(Named _ i) -> case i of
                                InstPhi _ -> True
                                _ -> False) bb


-- a Value, which can either be a constant, or a reference to an instruction.
data Value = ValueConstInt Int | ValueInstRef (Label Inst) deriving(Eq)

instance Pretty Value where
  pretty (ValueConstInt i) = pretty i <> pretty "#"
  pretty (ValueInstRef name) = pretty "%" <> pretty name

-- | Instructions that we allow within a basic block.
data Inst = InstAlloc
  | InstAdd Value Value
  | InstMul Value Value
  | InstL Value Value
  | InstAnd Value Value
  | InstLoad Value
  | InstStore Value Value
  | InstPhi (NE.NonEmpty (IRBBId, Value)) deriving(Eq)

-- | Given `Inst` (which is known to be a Phi node), get a `Value` which
-- | corresponds to the given `IRBBId`
getPhiValueForBB :: IRBBId -> Inst -> Maybe Value
getPhiValueForBB bbid phi@(InstPhi valList) = 
  case NE.filter ((==bbid) . fst) valList of
    [] -> Nothing
    [(_, v)] -> Just v
    xs -> error . docToString $ vcat $ 
        [pretty "Phi node should at most one copy of a predecessor BB, found:",
        pretty xs,
        pretty "Phi node:",
        pretty phi]
getPhiValueForBB _ inst =
  error . docToString $ vcat 
    [pretty "getPhiValueForBB should only be called on Phi. Found:",
     pretty inst]
-- | Map over the `Value`s in an Inst
mapInstValue :: (Value -> Value) -> Inst -> Inst
mapInstValue f inst = runIdentity $ forInstValue (Identity . f) inst

-- | TODO: use Uniplate.
-- | Run an effect `f` over the values of an instruction
forInstValue :: Applicative m => (Value -> m Value) -> Inst -> m Inst
forInstValue _ (InstAlloc) = pure InstAlloc
forInstValue f (InstAdd lhs rhs) = InstAdd <$> (f lhs) <*> (f rhs)
forInstValue f (InstMul lhs rhs) = InstMul <$> (f lhs) <*> (f rhs)
forInstValue f (InstL lhs rhs) = InstL <$> (f lhs) <*> (f rhs)
forInstValue f (InstAnd lhs rhs) = InstAnd <$> (f lhs) <*> (f rhs)
forInstValue f (InstLoad lhs) = InstLoad <$> f lhs
forInstValue f (InstStore lhs rhs) = InstStore <$> (f lhs) <*> (f rhs)
forInstValue f (InstPhi valList) = InstPhi <$> for valList (f' f) where
  f' :: Applicative m => (Value -> m Value)
      -> (IRBBId, Value)
      -> m (IRBBId, Value)
  f' f (irbbid, val) = liftA2 (,) (pure irbbid) (f val)

-- | Collect a monoidal Value over an Inst
foldMapInstValue :: Monoid m => (Value -> m) -> Inst -> m
foldMapInstValue f inst = execState final Monoid.mempty where
  -- go :: Value -> State m Value
  go v = do 
          modify (\m -> m Monoid.<> f v)
          return v

  -- final :: State m Inst
  final = (forInstValue go inst)


instance Pretty Inst where
  pretty (InstAlloc) = pretty "alloc"
  pretty (InstAdd l r) = pretty "add" <+> pretty l <+> pretty r
  pretty (InstMul l r) = pretty "mul" <+> pretty l <+> pretty r
  pretty (InstL l r) = pretty "lessthan" <+> pretty l <+> pretty r
  pretty (InstAnd l r) = pretty "and" <+> pretty l <+> pretty r
  pretty (InstLoad op) = pretty "load" <+> pretty op
  pretty (InstStore slot val) = pretty "store" <+> pretty val <+>
                                pretty "in" <+> pretty slot
  pretty (InstPhi philist) =
    pretty "Phi: " <+> hcat (punctuate comma (NE.toList (fmap (\(bbid, val) ->
                                brackets (pretty bbid <+> pretty val)) philist)))

-- | Return instructions are the only ones that can cause control flow
-- | between one basic block to another.
data RetInst =
  RetInstConditionalBranch Value IRBBId IRBBId |
  RetInstBranch IRBBId |
  RetInstTerminal |
  RetInstRet Value deriving(Eq)

instance Pretty RetInst where
  pretty (RetInstTerminal) = pretty "TERMINAL"
  pretty (RetInstBranch next) = pretty "branch" <+> pretty next
  pretty (RetInstConditionalBranch cond then' else') =
    pretty "branch if" <+> pretty cond <+>
    pretty "then" <+> pretty then' <+>
    pretty "else" <+> pretty else'
  pretty (RetInstRet val) = pretty "ret" <+> pretty val

-- | Run an effect `f` over the values of the return instruction
forRetInstValue :: Applicative m => (Value -> m Value) -> RetInst -> m RetInst
forRetInstValue _ RetInstTerminal = pure RetInstTerminal
forRetInstValue _ (RetInstBranch bbid) = pure (RetInstBranch bbid)
forRetInstValue f (RetInstConditionalBranch v t e) =
    RetInstConditionalBranch <$> f v <*> pure t <*> pure e
forRetInstValue f (RetInstRet v) = RetInstRet <$> f v

mapRetInstValue :: (Value -> Value) -> RetInst -> RetInst
mapRetInstValue f ret = runIdentity $ forRetInstValue (Identity . f) ret

-- | Run an effect `f` over the basic block IDs of the return instruction
forRetInstBBId :: Applicative m => (IRBBId -> m IRBBId) -> RetInst -> m RetInst
forRetInstBBId _ RetInstTerminal = pure RetInstTerminal
forRetInstBBId f (RetInstBranch bbid) =  (RetInstBranch <$> f bbid)
forRetInstBBId f (RetInstConditionalBranch v t e) =
    RetInstConditionalBranch <$> pure v <*> f t <*> f e
forRetInstBBId _ (RetInstRet v) = pure (RetInstRet v)

mapRetInstBBId :: (IRBBId -> IRBBId) -> RetInst -> RetInst
mapRetInstBBId f ret = runIdentity $ forRetInstBBId (Identity . f) ret


-- | Represents @a that is optionally named by a @Label a
data Named a = Named { namedName :: Label a, namedData :: a } deriving(Functor, Foldable, Traversable, Eq)

hasName :: (Label a) -> Named a -> Bool
hasName lbl named = namedName named == lbl


-- | Infix operator for @Named constructor
(=:=) :: Label a  -> a -> Named a
name =:= a = Named name a

instance Pretty a => Pretty (Named a) where
  pretty (Named name data') = pretty name <+> pretty ":=" <+> pretty data'


type IRProgram = Program (Named Inst) RetInst

-- | Replace all uses of an instruction in a program
replaceUsesOfInst :: Label Inst -> Value -> IRProgram -> IRProgram
replaceUsesOfInst instlbl newval program =
    mapProgramBBs fbb program  where
        replaceVal :: Value -> Value
        replaceVal (ValueInstRef ((== instlbl) -> True)) = newval
        replaceVal v = v

        finst :: Named Inst -> Named Inst
        finst = fmap (mapInstValue replaceVal)

        fretinst :: RetInst -> RetInst
        fretinst = mapRetInstValue replaceVal

        fbb :: IRBB -> IRBB
        fbb = mapBB finst fretinst

\end{code}