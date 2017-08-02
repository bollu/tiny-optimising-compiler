module TransformMem2Reg(constructDominatorTree, BBGraph(..), constructBBDominators) where
{-# LANGUAGE TupleSections #-}

import IR
import Data.Tree
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text.Prettyprint.Doc as PP
import Debug.Trace
import PrettyUtils
import Control.Monad.Reader
import Data.Traversable
import qualified Data.Monoid as Monoid

-- | adjacency list representation
newtype BBGraph = BBGraph { bbGraphEdges :: [(BBId, BBId)] }

instance Pretty BBGraph where
  pretty graph = 
    vcat [pretty "BB graph edges",
          (vcat . map (indent 4. pretty) . bbGraphEdges $ graph)]

-- | return predecessors
getPredecessors :: BBGraph -> BBId -> [BBId]
getPredecessors bbgraph bbid = [ src | (src, sink) <-(bbGraphEdges bbgraph), sink == bbid]

-- | Get the successors of this basic block
getBBSuccessors :: BasicBlock -> [BBId]
getBBSuccessors (BasicBlock { bbRetInst = RetInstTerminal}) = []
getBBSuccessors (BasicBlock { bbRetInst = RetInstBranch next}) = [next]
getBBSuccessors (BasicBlock { bbRetInst = RetInstConditionalBranch _ l r}) = [l, r]

mkBBGraph :: M.Map BBId BasicBlock -> BBGraph
mkBBGraph bbMap = BBGraph (M.foldMapWithKey makeEdges bbMap)  where

    -- Make the edges corresponding to basic block.
    makeEdges :: BBId -> BasicBlock -> [(BBId, BBId)]
    makeEdges bbid bb = map (\succ -> (bbid, succ)) (getBBSuccessors bb)

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


initialBBIdToDomSet :: EntryBBId ->  -- ^entry BB ID
                  [BBId] ->  -- ^All BB IDs
                  BBIdToDomSet
initialBBIdToDomSet entryid ids = M.fromList (mapEntry:mapAllExceptEntry) where
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

dominfoIterate :: EntryBBId -> -- ^Entry node ID
                 BBGraph -> -- ^Graph of BBs
                 BBIdToDomSet -> -- ^Previous dom info
                 BBIdToDomSet -- ^ New dom info
dominfoIterate entryid bbgraph prevdominfo =  M.mapWithKey computeNewDom prevdominfo where
  -- For the root node, DomSet_iplus1(root) = root
  -- For a non-root node, DomSet_iplus1(n) = intersect (forall p \in preds(n) DomSet_i(p)) U {n}
  computeNewDom :: BBId -> DomSet -> DomSet
  computeNewDom id old = if id == entryid then old else computeNewNonRootDom id
  -- compute the dom set of a node that is not the root
  computeNewNonRootDom :: BBId -> DomSet
  computeNewNonRootDom bbid = (combinePredDomSets ((getDoms . preds) bbid)) `S.union` (S.singleton bbid)

  -- predecessors of id
  preds :: BBId -> [BBId]
  preds bbid = getPredecessors bbgraph bbid

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
constructBBDominators :: IRProgram -> BBIdToDomSet
constructBBDominators program = getFirstAdjacentEqual iterations where
    -- iterations of domInfoIterate applied
    iterations :: [BBIdToDomSet]
    iterations = iterate (dominfoIterate entryid bbgraph) initdominfo

    -- graph structure
    bbgraph :: BBGraph
    bbgraph =  mkBBGraph (irProgramBBMap program)

    -- seed constructBBDominators
    initdominfo :: BBIdToDomSet
    initdominfo = initialBBIdToDomSet entryid bbids

    -- ID of the root node
    entryid :: BBId
    entryid = irProgramEntryBBId program

    -- list of all basic block IDs
    bbids :: [BBId]
    bbids = M.keys (irProgramBBMap program)


data DomTreeContext = DomTreeContext {
  ctxBBIdToDomSet :: BBIdToDomSet,
  ctxEntryId :: EntryBBId
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


foldMapM :: (Foldable t, Monad m, Monoid r) => t a -> (a -> m r) -> m r
foldMapM ta mfn = foldM (\monoid a -> (monoid Monoid.<>) <$> (mfn a)) mempty ta

newtype DomTree = DomTree { domTreeEdges :: [(BBId, BBId)] }

instance Pretty DomTree where
  pretty graph = 
    vcat [pretty "dom tree edges: ",
          (vcat . map (indent 4. pretty) . domTreeEdges $ graph)]

-- | internal reader that is not exported
createDominatorTree_ :: Reader DomTreeContext DomTree
createDominatorTree_ = do
  bbs <- reader (M.keys . ctxBBIdToDomSet)
  idoms <- foldMapM bbs (\bb -> do
                              mIdom <- getImmediateDominator bb
                              case mIdom of
                                Just idom -> return [(idom, bb)]
                                Nothing -> return [])
  return $ DomTree idoms




-- | Construct the Dominator tree from the dominator sets and the entry BB
constructDominatorTree :: M.Map BBId DomSet -> EntryBBId -> DomTree
constructDominatorTree bbidToDomSet entrybb  = runReader createDominatorTree_ (DomTreeContext bbidToDomSet entrybb)

