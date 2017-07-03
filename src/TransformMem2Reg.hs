module TransformMem2Reg where
import IR
import Data.Tree
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Graph as G
import Data.Text.Prettyprint.Doc as PP
import Debug.Trace
import PrettyUtils
import Control.Monad.Reader
import Data.Traversable

-- | Get the successors of this basic block
getBBSuccessors :: BasicBlock -> [BBId]
getBBSuccessors (BasicBlock { bbRetInst = RetInstTerminal}) = []
getBBSuccessors (BasicBlock { bbRetInst = RetInstBranch next}) = [next]
getBBSuccessors (BasicBlock { bbRetInst = RetInstConditionalBranch _ l r}) = [l, r]

mkBBGraph :: M.Map BBId BasicBlock -> (G.Graph, G.Vertex -> BBId)
mkBBGraph bbMap = let (g, vtoedge) = G.graphFromEdges' edges in (g, make_vtobbid vtoedge) where
    edges :: [(BasicBlock, BBId, [BBId])]
    edges = map makeEdge (M.toList bbMap)

    -- Make the edge corresponding to basic block.
    makeEdge :: (BBId, BasicBlock) -> (BasicBlock, BBId, [BBId])
    makeEdge (bbid, bb) = (bb, bbid, getBBSuccessors bb)

    -- Pull out only BBId from edge
    make_vtobbid :: (G.Vertex -> (BasicBlock, BBId, [BBId])) -> G.Vertex -> BBId
    make_vtobbid vtoedge v = let (_, bbid, _) = vtoedge v in bbid

-- a dominator tree is a tree of basic blocks
newtype DominatorTree  = Tree BasicBlock

-- BBId of the root node.
type EntryBBId = BBId


-- | Set of nodes that dominate a node.
type DomSet =  S.Set BBId

instance Pretty a => Pretty (S.Set a) where
  pretty = pretty . S.toList

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  pretty m = vsep ( map (\(k, v) -> pretty k <+> pretty ":" <+> pretty v) (M.toList m))

-- | Map from a node to the set of nodes that dominate it
type BBIdToDomSet = M.Map BBId DomSet


initialDomSetMap :: EntryBBId ->  -- ^entry BB ID
                  [BBId] ->  -- ^All BB IDs
                  BBIdToDomSet
initialDomSetMap entryid ids = M.fromList (mapEntry:mapAllExceptEntry) where
  -- entry block only dominantes itself
  mapEntry :: (BBId, DomSet)
  mapEntry = (entryid, S.fromList [entryid])
  -- IDs other than the entry block
  nonEntryIds :: [BBId]
  nonEntryIds = filter (/= entryid) ids
  -- list mapping basic block Ids to dominance sets
  mapAllExceptEntry ::  [(BBId, DomSet)]
  mapAllExceptEntry =  zip nonEntryIds (repeat allDomSet)
  -- all nodes in the dom set
  allDomSet :: DomSet
  allDomSet = S.fromList ids



-- Get the predecessors of 'BBId' in 'Graph'
getPredecessors :: G.Graph -> (G.Vertex -> BBId) ->  BBId -> [BBId]
getPredecessors bbgraph vtobbid bbid = map getFrom (filter isToId (G.edges bbgraph)) where
  -- Get the basic block this edge is from
  getFrom :: G.Edge -> BBId
  getFrom = vtobbid . fst

  -- Check if the edge is to the ID we care about
  isToId :: G.Edge -> Bool
  isToId = (== bbid) . vtobbid . snd

dominfoIterate :: EntryBBId -> -- ^Entry node ID
                 G.Graph -> -- ^Graph of BBs
                   (G.Vertex -> BBId) -> -- ^Graph vertex to BBId
                 BBIdToDomSet -> -- ^Previous dom info
                 BBIdToDomSet -- ^ New dom info
dominfoIterate rootid bbgraph vtobbid prevdominfo =  M.mapWithKey computeNewDom prevdominfo where
  -- For the root node, DomSet_iplus1(root) = root
  -- For a non-root node, DomSet_iplus1(n) = intersect (forall p \in preds(n) DomSet_i(p)) U {n}
  computeNewDom :: BBId -> DomSet -> DomSet
  computeNewDom id old = if id == rootid then old else computeNewNonRootDom id
  -- compute the dom set of a node that is not the root
  computeNewNonRootDom :: BBId -> DomSet
  computeNewNonRootDom bbid = (combinePredDomSets ((getDoms . preds) bbid)) `S.union` (S.singleton bbid)

  -- predecessors of id
  preds :: BBId -> [BBId]
  preds bbid = trace ("preds(" ++ (prettyableToString bbid) ++ ")" ++ "\n" ++ docToString (vcat . map (indent 4 . pretty) $ p) ++ "\n---\n") p where
                p = getPredecessors bbgraph vtobbid bbid

  -- combine all predecessor dom sets by intersecting them
  combinePredDomSets :: [DomSet] -> DomSet
  combinePredDomSets [] = error "unreachable node in domset"
  combinePredDomSets ds = foldl1 S.intersection ds

  -- get dominators of ids
  getDoms :: [BBId] -> [DomSet]
  getDoms bbids = map (prevdominfo M.!) bbids

getFirstAdjacentEqual :: Eq a => [a] -> a
getFirstAdjacentEqual as = fst . head $ dropWhile (\(a, a') -> a /= a') (zip as (tail as))

-- Map each basic block to the set of basic blocks that dominates it
dominfo :: IRProgram -> BBIdToDomSet
dominfo program = getFirstAdjacentEqual iterations where
    -- iterations of domInfoIterate applied
    iterations :: [BBIdToDomSet]
    iterations = iterate (dominfoIterate rootid (fst graphinfo) (snd graphinfo)) initdominfo

    -- graph structure
    graphinfo :: (G.Graph, G.Vertex -> BBId)
    graphinfo =  mkBBGraph (irProgramBBMap program)

    -- seed domInfo
    initdominfo :: BBIdToDomSet
    initdominfo = initialDomSetMap rootid bbids

    -- ID of the root node
    rootid :: BBId
    rootid = irProgramEntryBBId program

    -- list of all basic block IDs
    bbids :: [BBId]
    bbids = M.keys (irProgramBBMap program)



data DomTreeContext = DomTreeContext {
  ctxBBIdToDomSet :: BBIdToDomSet,
  ctxEntryId :: BBId
}

-- | Returns the dominators of BBId
getBBDominators :: BBId -> Reader DomTreeContext DomSet
getBBDominators bbid = do
  bbIdToDomSet <- reader ctxBBIdToDomSet
  return $ bbIdToDomSet M.! bbid

-- | Returns the struct dominators of BBId
getBBStrictDominators :: BBId -> Reader DomTreeContext DomSet
getBBStrictDominators bbid = S.filter (/= bbid) <$> (getBBDominators bbid)


-- | Returns whether y dominates x
doesDominate :: BBId -> BBId -> Reader DomTreeContext Bool
doesDominate x y = do
    bbIdToDomSet <- reader ctxBBIdToDomSet
    return $ x `S.member` (bbIdToDomSet M.! y)

-- | Run a forall in a traversable for a monadic context
allTraversable :: (Foldable t, Traversable t, Monad m) => t a -> (a -> m Bool) -> m Bool
allTraversable ta mpred = (foldl (&&) True) <$> (forM ta mpred)

-- | Return if the BBId is dominated by all bbs *other than itself* in others
isDominatedByAllOthers :: [BBId] -> BBId -> Reader DomTreeContext Bool
isDominatedByAllOthers others self =
  allTraversable others(\other -> if other == self then return True
                                                    else doesDominate other self)

-- | Returns the immediate dominator if present, otherwise returns Nothing
getImmediateDominator :: BBId -> Reader DomTreeContext (Maybe BBId)
getImmediateDominator bbid = do
    entryid <- reader ctxEntryId
    if entryid == bbid then
      return Nothing
    else do
            strictDoms <- S.toList <$> getBBStrictDominators bbid
            idoms <-  filterM (isDominatedByAllOthers strictDoms) strictDoms
            case idoms of
                    [idom] -> return (Just idom)
                    [] -> return Nothing
                    _ -> error $ "*** found more than one idom:\n" ++ show (prettyableToString idoms)



