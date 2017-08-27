\begin{code}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
module OrderedMap(OrderedMap,
  fromList,
  size,
  adjust,
  adjustWithKey,
  insert,
  insertWith,
  elems,
  toList,
  keys,
  editKeys,
  (!),
  union,
  fromListWith,
  foldMapWithKey,
  foldlWithKey,
  mapWithKey,
  OrderedMap.lookup,
  delete) where
import qualified Data.Map.Strict as M
import Control.Applicative(liftA2)
import qualified Control.Arrow as A
import Data.Monoid
import PrettyUtils
import Data.Text.Prettyprint.Doc
import qualified Data.List as L

-- At some point, I need this. This is more convenient than overloading the key to store the insertion time.
-- | A dictionary that orders elements by insertion time
data OrderedMap k v = OrderedMap { map' :: M.Map k v, order :: [k] } deriving(Show, Functor, Eq)

instance (Ord k, Pretty k) => Foldable (OrderedMap k) where
 foldMap f omap = foldMap f (map snd . toList $ omap)

instance (Ord k, Pretty k) => Traversable (OrderedMap k) where
 traverse f omap = fmap fromList (traverse ((\(k, v) -> liftA2 (,) (pure k) (f v)))  (toList omap))

instance (Ord k, Pretty k, Pretty v) => Pretty (OrderedMap k v) where
  pretty (OrderedMap _ []) = pretty "empty map"
  pretty ok = indent 2 (vcat (map pkv (toList ok))) where
    pkv :: (Pretty k, Pretty v) => (k, v) -> Doc ann
    pkv (k, v) =  pretty k <+> pretty " => " <+> pretty v

instance Ord k => Monoid (OrderedMap k v) where
  mempty :: OrderedMap k v
  mempty = OrderedMap mempty mempty

  mappend :: OrderedMap k v -> OrderedMap k v -> OrderedMap k v
  mappend (OrderedMap m o) (OrderedMap m' o') = OrderedMap (m `mappend` m') (o `mappend` o')

liftMapEdit_ :: (M.Map k v -> M.Map k v') -> OrderedMap k v -> OrderedMap k v'
liftMapEdit_ f (OrderedMap map' order) = OrderedMap (f map') order

liftMapExtract_ :: (M.Map k v -> a) -> OrderedMap k v -> a
liftMapExtract_ f (OrderedMap map' _) = f map'

-- | NOTE: this will maintain the order of insertion. Elements that are inserted
-- | later are returned later in the `keys`, `elems`.
insert  :: Ord k => k -> a -> OrderedMap k a -> OrderedMap k a
insert k a om@OrderedMap{..} = case (liftMapExtract_ (M.lookup k)) om of
            Nothing -> OrderedMap (M.insert k a map') (order ++ [k])
            -- If the key already exists, keep the old order
            _ -> OrderedMap (M.insert k a map') (order)

-- | NOTE: this will maintain the order of insertion. Elements that are inserted
-- | later are returned later in the `keys`, `elems`.
insertWith :: Ord k => (a -> a -> a) -> k -> a -> OrderedMap k a -> OrderedMap k a
insertWith combiner k a om@OrderedMap{..} =
  case (liftMapExtract_ (M.lookup k)) om of
    Nothing -> OrderedMap (M.insertWith combiner k a map') (order ++ [k])
    -- If the key already exists, keep the old order
    _ -> OrderedMap (M.insertWith combiner k a map') (order)

lookup :: Ord k => k -> OrderedMap k a -> Maybe a
lookup k = liftMapExtract_ (M.lookup k)

fromList :: Ord k => [(k, a)] -> OrderedMap k a
fromList kv = OrderedMap (M.fromList kv)  (map fst kv)

size :: OrderedMap k a -> Int
size = liftMapExtract_ M.size

keys :: OrderedMap k a -> [k]
keys  = order

index_ :: (Ord k) => OrderedMap k a -> k -> a
index_ omap k = case OrderedMap.lookup k omap of
            Just a -> a
            Nothing -> error . docToString $
                         vcat [pretty "Omap is in inconstent state."]

elems :: (Ord k, Pretty k, Pretty a) => OrderedMap k a -> [a]
elems omap = map (index_ omap) (keys omap) where

union :: (Eq k, Ord k) => OrderedMap k a -> OrderedMap k a -> OrderedMap k a
union (OrderedMap{order=o1, map'=m1}) (OrderedMap{order=o2, map'=m2}) =
  OrderedMap{map'=m1 `M.union` m2, order=L.nub(o1++o2)}

-- | Return the list of key value pairs in the order of insertion.
toList :: (Ord k) => OrderedMap k a -> [(k, a)]
toList omap = map (\k -> (k, index_ omap k)) (keys omap)

adjust :: Ord k => (a -> a) -> k -> OrderedMap k a -> OrderedMap k a
adjust f k = liftMapEdit_ (M.adjust f k)

adjustWithKey :: Ord k => (k -> a -> a) -> k -> OrderedMap k a -> OrderedMap k a
adjustWithKey f k = liftMapEdit_ (M.adjustWithKey f k)

(!) :: (Ord k, Pretty k, Pretty a) => OrderedMap k a -> k -> a
ok ! k =
  case (OrderedMap.lookup k ok) of
           Just a -> a
           Nothing -> error . docToString $
               vcat [pretty "key missing, has no value associated with it: " <+> pretty k,
                     pretty "map:",
                     indent 4 (pretty ok),
                     pretty "---"]

foldMapWithKey :: Monoid m => (k -> a -> m) -> OrderedMap k a -> m
foldMapWithKey f = liftMapExtract_ (M.foldMapWithKey f)

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> OrderedMap k a
fromListWith f kvs = OrderedMap {order=fmap fst kvs, map'=M.fromListWith f kvs}

foldlWithKey :: (a -> k -> b -> a) -> a -> OrderedMap k b -> a
foldlWithKey f a = liftMapExtract_ (M.foldlWithKey f a)

mapWithKey :: (k -> a -> b) -> OrderedMap k a -> OrderedMap k b
mapWithKey f = liftMapEdit_ (M.mapWithKey f)

-- | Change the keys of the map, without changing the order.
editKeys :: (Ord k, Ord k') => (k -> k') -> OrderedMap k a -> OrderedMap k' a
editKeys f = fromList . map (f A.*** id) . toList


delete :: Ord k =>  k -> OrderedMap k a -> OrderedMap k a
delete key omap@OrderedMap{..} = OrderedMap {order=L.delete key order, map'=M.delete key map' }
\end{code}
