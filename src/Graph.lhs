<h1> Graph </h1>
In this module, we define a simple `graph` structure that can be used
as:

- an undirected.
- a directed graph.
- a tree.

Ideally, we would use some sort of phantom-type mechanism to distinguish
between the two, that is `Graph Undirected a` and `Graph Directed a`, but
oh well `:)`.



\begin{code}
{-# LANGUAGE ViewPatterns #-}

module Graph where
import Data.List(nub)
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils
import Data.Maybe (maybeToList)
import qualified OrderedMap as M
import qualified Data.Set as S

-- | Represents a graph with `a` as a vertex ID type
newtype Graph a = Graph { edges :: [(a, a)] }

instance Pretty a => Pretty (Graph a) where
  pretty graph =
    vcat [pretty "BB graph edges",
          (vcat . map (indent 4 . pretty) . edges $ graph)]

-- | return predecessors of a node
getPredecessors :: Eq a => Graph a -> a -> [a]
getPredecessors g bbid = [ src | (src, sink) <- (edges g), sink == bbid]

-- | Returns the children of an element in a dom tree
-- | This returns only the immediate children.
getImmediateChildren :: Eq a => Graph a -> a -> [a]
getImmediateChildren (Graph edges) a = [dest | (src, dest) <- edges, src==a]

-- | Return all the vertices of the subgraph
getAllChildren :: Eq a => Graph a -> a -> [a]
getAllChildren tree@(Graph edges) a =
  a:(curChilds >>= (getAllChildren tree)) where
  curChilds = getImmediateChildren tree a

-- | Return the set of vertices in DomTree
vertices :: Eq a => Graph a  -> [a]
vertices (Graph edges) = nub (map fst edges ++ map snd edges)

-- | Colors are assigned from [1..NGraphColors]
type GraphColor = Int
type NGraphColors = Int

_greedyColorGraph :: Ord a => Graph a -- ^ Graph 
                            -> S.Set a -- ^ Set of vertices
                            -> M.OrderedMap a (Maybe GraphColor) -- ^ Mapping from vertices to colors
                            -> NGraphColors -- ^ Total number of graph colors available
                            -> M.OrderedMap a (Maybe GraphColor) -- ^ Final colored graph
_greedyColorGraph _ (null -> True) coloring ncolors = coloring
_greedyColorGraph g vs@(S.elemAt 0 -> v) coloring ncolors  =
    _greedyColorGraph g vs' coloring' ncolors where
        -- adjacent vertices
        adjvs = (getPredecessors g v)

        -- colors of adjacent vertices
        adjColors :: [GraphColor]
        adjColors = mconcat $ fmap (\v -> case (v `M.lookup` coloring) of
                                            Just (Just c) -> [c]
                                            _ -> []) adjvs

        -- largest color
        largestAdjColor = case adjColors of
                            [] -> 0
                            xs -> maximum xs

        -- Leave it uncolored it we can't find a color
        coloring' = if largestAdjColor == ncolors
                then M.insert v Nothing coloring
                else M.insert v (Just (largestAdjColor + 1)) coloring

        -- remove vertex we currently processed
        vs' = S.deleteAt 0 vs


-- | Color the graph greedily and return the mapping of colors
greedyColorGraph :: Ord a => NGraphColors -> Graph a -> M.OrderedMap a (Maybe Int)
greedyColorGraph ngraphcolors g =
    _greedyColorGraph g (S.fromList (vertices g))
                      mempty ngraphcolors

\end{code}
