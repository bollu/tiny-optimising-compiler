
\begin{code}
-- Pretty Utils
-- ============
instance Pretty a => Pretty (S.Set a) where
  pretty s = case S.toList s of
               [] -> pretty "emptyset"
               xs -> indent 1 $ vcat $ [pretty "{"] ++ (map pretty xs)  ++ [pretty "}"]

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  pretty m = 
    if M.null m 
       then pretty "emptymap" 
       else (indent 1 (vcat $ [pretty "(" <> pretty k <+> pretty "->" <+> (pretty v) <> pretty ")" | (k, v) <- M.toList m]))

-- Lattice theory
-- ==============
-- top = join of all elements
class SemiJoin a where
  join :: a -> a -> a
  top :: a

-- bottom = meet of all elements 
class SemiMeet a where
  meet :: a -> a -> a
  bottom :: a

class (SemiJoin a, SemiMeet a) => Lattice a

instance SemiJoin a => SemiJoin (Maybe a) where
  top = Just top

  join Nothing a = a
  join a Nothing = a
  join (Just a) (Just b) = Just (join a b)

instance SemiMeet a => SemiMeet (Maybe a) where
  bottom = Nothing

  meet Nothing _ = Nothing
  meet _ Nothing = Nothing
  meet (Just a) (Just b) = Just (meet a b)

instance (SemiJoin a, SemiJoin b) => SemiJoin (a, b) where
  top = (top, top)
  join (a, b) (a', b') = (a `join` a', b `join` b')

instance (SemiMeet a, SemiMeet b) => SemiMeet (a, b) where
  bottom = (bottom, bottom)
  meet (a, b) (a', b') = (a `meet` a', b `meet` b')

instance (Lattice a, Lattice b) => Lattice (a, b)

data LiftedLattice a = LL !a | LLBot | LLTop deriving(Eq, Ord, Functor)

instance Pretty a => Pretty (LiftedLattice a) where
  pretty (LL a) = pretty a
  pretty LLBot = pretty "_|_"
  pretty LLTop = pretty "T"

instance Eq a => SemiJoin (LiftedLattice a) where
  top = LLTop

  join LLBot a = a
  join a LLBot = a
  join LLTop _ = LLTop
  join _ LLTop = LLTop
  join (LL a) (LL b) = if a == b then LL a else LLTop

instance Eq a => SemiMeet (LiftedLattice a) where
  bottom = LLBot

  meet LLBot _ = LLBot
  meet _ LLBot = LLBot
  meet a LLTop = a
  meet LLTop a = a
  meet (LL a) (LL b) = if a == b then LL a else LLBot

instance Eq a => Lattice (LiftedLattice a)



liftLL2 :: (a -> b -> c) -> LiftedLattice a -> LiftedLattice b -> LiftedLattice c
liftLL2 f LLTop _ = LLTop
liftLL2 f _ LLTop = LLTop
liftLL2 f LLBot _ = LLBot
liftLL2 f _ LLBot  = LLBot
liftLL2 f (LL a) (LL b) = LL (f a b)

instance Show a => Show (LiftedLattice a) where
  show LLBot = "_|_"
  show LLTop = "T"
  show (LL a) = show a


class Lattice a => BooleanAlgebra a where
  complement :: a -> a

-- implication in the boolean algebra
imply :: BooleanAlgebra a => a -> a -> a
imply a b = (complement a) `join` b

-- symbol
(===>) :: BooleanAlgebra a => a -> a -> a
(===>) = imply


-- Adjoin a top element 
data ToppedLattice a = TLTop | TL !a deriving (Eq, Ord, Functor)

instance Show a => Show (ToppedLattice a) where
  show TLTop = "T"
  show (TL a) = show a

data BottomedLattice a = TLBot | TB !a deriving(Eq, Ord, Functor)

instance Show a => Show (BottomedLattice a) where
  show TLBot = "_|_"
  show (TB a) = show a


-- A map based representation of a function (a -> b), which on partial
-- missing keys returns _|_
data SemiMeetMap k v = LM !(M.Map k v)  deriving (Eq, Ord, Functor)

-- Insert a regular value into a lattice map
lminsert :: Ord k => k -> v -> SemiMeetMap k v -> SemiMeetMap k v
lminsert k v (LM m) = LM $ M.insert k v m

-- pointwise produce of two lattice maps
-- If a value is missing in either lattice, put a bottom in its place
lmproduct :: (SemiMeet v, SemiMeet w, Ord k) => SemiMeetMap k v -> SemiMeetMap k w -> SemiMeetMap k (v, w)
lmproduct (LM m) (LM m') = let
  missingm' = M.mapMissing (\k w -> bottom)
  missingm =  M.mapMissing (\k v -> bottom)
  merger = M.zipWithMatched (\k tx ty -> (tx, ty))
  in LM $ M.merge missingm' missingm merger m m'

adjust :: Ord k => k -> (v -> v) -> SemiMeetMap k v -> SemiMeetMap k v
adjust k f (LM m) = LM $ M.adjust f k m

(!!#!) :: (Ord k, SemiMeet v) => SemiMeetMap k v -> k -> v
(!!#!) (LM m) k = case m M.!? k of
                   Just v -> v
                   Nothing -> bottom


(!!#?) :: Ord k => SemiMeetMap k v -> k -> Maybe v
(!!#?) (LM m) k = m M.!? k 

lmfromlist :: Ord k => [(k, v)] -> SemiMeetMap k v
lmfromlist kvs = LM $ M.fromList [(k, v) | (k, v) <- kvs]

lmempty :: SemiMeetMap k v 
lmempty = LM $ M.empty

lmtolist :: Ord k => SemiMeetMap k v -> [(k, v)]
lmtolist (LM m) = M.toList m

instance (Ord k, Show k, Show v, Pretty k, Pretty v) => Show (SemiMeetMap k v) where
  show (LM m) = show $ [(k, m !!# k) | k <- M.keys m]


instance (Ord k, Pretty k, Pretty v) => Pretty (SemiMeetMap k v) where
  pretty (LM m) =  pretty m -- vcat $ [pretty k <+> pretty "->" <+> pretty (m !!# k) | k <- M.keys m]

instance SemiMeet v => SemiMeet (SemiMeetMap k v) where
  bottom = LM M.empty
  meet _ _ = error "TODO: define meet"

\end{code}

<h1> Helper to repeat till fixpoint </h1>

\begin{code}
repeatTillFix :: (Eq a) =>  (a -> a) -> a -> a
repeatTillFix f a =
  let a' = f a in
  if a == a' then a else repeatTillFix f a'


-- repeat till fixpoint, or the max count
repeatTillFixDebug :: Eq a => Int -> (a -> a) -> a -> a
repeatTillFixDebug 0 f a = a
repeatTillFixDebug n f a = 
  let a' = f a in if a' == a then a else repeatTillFixDebug (n - 1) f a'


repeatTillFixDebugTrace :: Eq a => Int -> (a -> a) -> a -> [a]
repeatTillFixDebugTrace 0 f a = [a]
repeatTillFixDebugTrace n f a = 
  let a' = f a in if a' == a then [a] else a:repeatTillFixDebugTrace (n - 1) f a'

repeatTillFixDebugTraceM :: (Monad m) => Int -> (a -> a -> Bool) -> (a -> m a) -> a -> m [a]
repeatTillFixDebugTraceM 0 eqf f a = return [a]
repeatTillFixDebugTraceM n eqf f a = do
  a' <- f a
  if eqf a a' 
  then return [a]
  else do
    as <- repeatTillFixDebugTraceM (n - 1) eqf f a'
    return (a' : as)
\end{code}
