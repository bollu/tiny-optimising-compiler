module TransformMem2Reg where
import IR
import Data.Tree
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Graph as G
import Data.Text.Prettyprint.Doc as PP
import Debug.Trace
import qualified Data.Text.Lazy as L
import Data.Text.Prettyprint.Doc.Render.Text
docToText :: Doc ann -> L.Text
docToText doc = renderLazy (layoutPretty defaultLayoutOptions doc)

docToString :: Doc ann -> String
docToString = L.unpack . docToText
prettyableToString :: Pretty a => a -> String
prettyableToString  a = docToString (pretty a)

-- | Get the successors of this basic block
getBBSuccessors :: BasicBlock -> [BBId]
getBBSuccessors (BasicBlock { bbRetInst = RetInstTerminal}) = []
getBBSuccessors (BasicBlock { bbRetInst = RetInstBranch next}) = [next]
getBBSuccessors (BasicBlock { bbRetInst = RetInstConditionalBranch _ l r}) = [l, r]

mkBBGraph :: M.Map BBId BasicBlock -> (G.Graph, G.Vertex -> BBId)
mkBBGraph bbMap = let (g, vtoedge,  _ ) = G.graphFromEdges edges in (g, make_vtobbid vtoedge) where
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


-- | Set of nodes that dominate a node.
type DomSet =  S.Set BBId

instance Pretty a => Pretty (S.Set a) where
  pretty = pretty . S.toList

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  pretty m = vsep ( map (\(k, v) -> pretty k <+> pretty ":" <+> pretty v) (M.toList m))

-- | Map from a node to the set of nodes that dominate it
type DomInfo = M.Map BBId DomSet


initialDomInfo :: BBId ->  -- ^entry BB ID
                  [BBId] ->  -- ^All BB IDs
                  DomInfo
initialDomInfo entryid ids = M.fromList (mapEntry:mapAllExceptEntry) where
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

dominfoIterate :: BBId -> -- ^Root node ID
                 G.Graph -> -- ^Graph of BBs
                   (G.Vertex -> BBId) -> -- ^Graph vertex to BBId
                 DomInfo -> -- ^Previous dom info
                 DomInfo -- ^ New dom info
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


-- Drop all elements till the list has two adjacent equal elements
dropTillEqual :: Eq a => [a] -> a
dropTillEqual xs = fst (head (dropWhile (\x -> fst x /= snd x) (zip xs (tail xs))))


dominfo_ :: IRProgram -> [DomInfo]
dominfo_ program = iterations where
    -- iterations of domInfoIterate applied
    iterations :: [DomInfo]
    iterations = iterate (dominfoIterate rootid (fst graphinfo) (snd graphinfo)) initdominfo

    -- graph structure
    graphinfo :: (G.Graph, G.Vertex -> BBId)
    graphinfo =  mkBBGraph (irProgramBBMap program)

    -- seed domInfo
    initdominfo :: DomInfo
    initdominfo = initialDomInfo rootid bbids

    -- ID of the root node
    rootid :: BBId
    rootid = irProgramEntryBBId program

    -- list of all basic block IDs
    bbids :: [BBId]
    bbids = M.keys (irProgramBBMap program)

-- Map each basic block to the set of basic blocks that dominates it
dominfo :: IRProgram -> DomInfo
dominfo program = dropTillEqual iterations where
    -- iterations of domInfoIterate applied
    iterations :: [DomInfo]
    iterations = iterate (dominfoIterate rootid (fst graphinfo) (snd graphinfo)) initdominfo

    -- graph structure
    graphinfo :: (G.Graph, G.Vertex -> BBId)
    graphinfo =  mkBBGraph (irProgramBBMap program)

    -- seed domInfo
    initdominfo :: DomInfo
    initdominfo = initialDomInfo rootid bbids

    -- ID of the root node
    rootid :: BBId
    rootid = irProgramEntryBBId program

    -- list of all basic block IDs
    bbids :: [BBId]
    bbids = M.keys (irProgramBBMap program)

