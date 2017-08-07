{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TransformMem2Reg(constructDominatorTree,
    CFG(..),
    mkBBGraph,
    constructBBDominators,
    getAllChildren,
    getDominanceFrontier,
    transformMem2Reg) where

import IR
import Data.Tree
import Data.List(nub)
import qualified Data.Set as S
import qualified OrderedMap as M
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils
import Control.Monad.Reader
import Data.Traversable
import qualified Data.Monoid as Monoid
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Control.Monad.State.Strict

-- | Represents a graph with `a` as a vertex ID type
newtype Graph a = Graph { edges :: [(a, a)] }

instance Pretty a => Pretty (Graph a) where
  pretty graph =
    vcat [pretty "BB graph edges",
          (vcat . map (indent 4 . pretty) . edges $ graph)]

-- | The control flow graph, which is a graph of basic blocks
type CFG = Graph BBId

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

-- | Get the successors of this basic block
getBBSuccessors :: BasicBlock -> [BBId]
getBBSuccessors (BasicBlock { bbRetInst = RetInstTerminal}) = []
getBBSuccessors (BasicBlock { bbRetInst = RetInstRet _}) = []
getBBSuccessors (BasicBlock { bbRetInst = RetInstBranch next}) = [next]
getBBSuccessors (BasicBlock { bbRetInst = RetInstConditionalBranch _ l r}) = [l, r]

mkBBGraph :: M.OrderedMap BBId BasicBlock -> CFG
mkBBGraph bbMap = Graph (M.foldMapWithKey makeEdges bbMap)  where

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

-- | Map from a node to the set of nodes that dominate it
type BBIdToDomSet = M.OrderedMap BBId DomSet


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
                 CFG -> -- ^Graph of BBs
                 BBIdToDomSet -> -- ^Previous dom info
                 BBIdToDomSet -- ^ New dom info
dominfoIterate entryid cfg prevdominfo =  M.mapWithKey computeNewDom prevdominfo where
  -- For the root node, DomSet_iplus1(root) = root
  -- For a non-root node, DomSet_iplus1(n) = intersect (forall p \in preds(n) DomSet_i(p)) U {n}
  computeNewDom :: BBId -> DomSet -> DomSet
  computeNewDom id old = if id == entryid then old else computeNewNonRootDom id
  -- compute the dom set of a node that is not the root
  computeNewNonRootDom :: BBId -> DomSet
  computeNewNonRootDom bbid = (combinePredDomSets ((getDoms . preds) bbid)) `S.union` (S.singleton bbid)

  -- predecessors of id
  preds :: BBId -> [BBId]
  preds bbid = getPredecessors cfg bbid

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
    iterations = iterate (dominfoIterate entryid cfg) initdominfo

    -- graph structure
    cfg :: CFG
    cfg =  mkBBGraph (irProgramBBMap program)

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

-- newtype DomTree = DomTree { domTreeEdges :: [(BBId, BBId)] }
type DomTree = Graph BBId

-- | internal reader that is not exported
createDominatorTree_ :: Reader DomTreeContext DomTree
createDominatorTree_ = do
  bbs <- reader (M.keys . ctxBBIdToDomSet)
  idoms <- foldMapM bbs (\bb -> do
                              mIdom <- getImmediateDominator bb
                              case mIdom of
                                Just idom -> return [(idom, bb)]
                                Nothing -> return [])
  return $ Graph idoms




-- | Construct the Dominator tree from the dominator sets and the entry BB
constructDominatorTree :: M.OrderedMap BBId DomSet -> EntryBBId -> DomTree
constructDominatorTree bbidToDomSet entrybb  = runReader createDominatorTree_ (DomTreeContext bbidToDomSet entrybb)




-- | The `dominates` relation. Given two BB Ids, returns whether `a` dominates `b`
dominates :: DomTree -> BBId -> BBId -> Bool
dominates tree a b = b `elem` (getAllChildren tree a)

-- | Strictly dominates relation. `A` strictlyDom `B` iff `A` Dom `B` and `A` /= `B`
strictlyDominates :: DomTree -> BBId -> BBId -> Bool
strictlyDominates domtree a b = dominates domtree a b && a /= b

-- | Get the list of dominance frontiers of a given BB
-- | "The dominance frontier of a node d is the set of all nodes n such that d
-- |  dominates an immediate predecessor of n, but d does not strictly dominate
-- |  n. It is the set of nodes where d's dominance stops."
-- | TODO: think anbout why *an immediate preceseeor*, not *all immediate ...*.
getDominanceFrontier :: DomTree -> CFG -> BBId -> [BBId]
getDominanceFrontier tree@(Graph domedges) cfg cur =
  [bb | bb <- vertices tree, any (dominates tree cur) (preds bb) , not (strictlyDominates tree cur bb)] where
  preds bb = (getPredecessors cfg bb)


-- Get the names of all values allocated in a basic block
getBBVarUses :: BasicBlock -> [Label Inst]
getBBVarUses (BasicBlock{bbInsts=bbInsts}) =
  bbInsts >>= \(Named name inst) -> case inst of
                                        InstAlloc -> [name]
                                        InstStore (ValueInstRef slot) _ -> [slot]
                                        _ -> []

-- | Adjust possibly many keys
adjustWithKeys :: (Ord k, Foldable t) => (k -> a -> a) -> t k -> M.OrderedMap k a -> M.OrderedMap k a
adjustWithKeys f ks m = foldl (\m k -> M.adjustWithKey f k m) m ks


unsafeToNonEmpty :: [a] -> NE.NonEmpty a
unsafeToNonEmpty [] = error "unable to convert empty list to non empty"
unsafeToNonEmpty (x:xs) = x NE.:| xs

-- | For a given basic block, insert a phi node at the top
insertPhiNodeCallback_ :: CFG -> Label Inst -> BBId -> BasicBlock -> BasicBlock
insertPhiNodeCallback_ cfg lbl bbid bb@(BasicBlock{..}) =
    bb {bbInsts=bbInsts'} where
    bbInsts' :: [Named Inst]
    bbInsts' = (lbl =:= phi):bbInsts

    phi :: Inst
    phi = InstPhi . unsafeToNonEmpty $ (zip ((getPredecessors cfg bbid)) (repeat (ValueInstRef lbl)))


-- | Place Phi nodes for a given instruction at a set of start CFGs. Initially, they should be the
-- |  set of nodes that store to the original value
placePhiNodesForAlloc_ :: Label Inst -- ^Name of the original value
                          -> S.Set BBId -- ^BBs to process
                          -> S.Set BBId -- ^BBs that are already processed
                          -> CFG -- ^The CFG of the function
                          -> DomTree -- ^The dominator tree of the function
                          -> M.OrderedMap BBId BasicBlock -- ^Function body
                          -> M.OrderedMap BBId BasicBlock
placePhiNodesForAlloc_ name curbbs processed cfg domtree bbmap =
    if null (curbbs)
    then bbmap
    else (placePhiNodesForAlloc_ name curbbs' processed' cfg domtree bbmap')  where
                cur :: BBId
                cur = S.elemAt 0 curbbs

                -- | For every basic block in the dominance frontier, insert a phi node.
                bbmap' :: M.OrderedMap BBId BasicBlock
                bbmap' = adjustWithKeys (insertPhiNodeCallback_ cfg name) curfrontier bbmap

                curbbs' :: S.Set BBId
                curbbs' = (curbbs `S.union` curfrontier) S.\\ processed'

                curfrontier ::  S.Set BBId
                curfrontier = (S.fromList $ getDominanceFrontier domtree cfg cur) S.\\ processed'

                processed' :: S.Set BBId
                processed' = (S.insert cur processed)

mapReverse :: (Pretty k, Pretty a, Ord k, Ord a) => M.OrderedMap k [a] -> M.OrderedMap a [k]
mapReverse m = M.fromListWith (++) [(a, [k]) | (k, as) <- M.toList m, a <- as]

-- References: http://www.cs.is.noda.tus.ac.jp/~mune/keio/m/ssa2.pdf
placePhiNodes_ :: CFG -> DomTree -> M.OrderedMap BBId BasicBlock -> M.OrderedMap BBId BasicBlock
placePhiNodes_ cfg domtree initbbmap =
    M.foldlWithKey (\curbbmap name bbids -> placePhiNodesForAlloc_ name (S.fromList bbids) mempty cfg domtree curbbmap) initbbmap usesToBBIds  where
      bbIdToUses :: M.OrderedMap BBId [Label Inst]
      bbIdToUses = fmap getBBVarUses initbbmap

      usesToBBIds :: M.OrderedMap (Label Inst) [BBId]
      usesToBBIds = mapReverse bbIdToUses

-- | Find wherever this variable is "declared". An Alloca or a Phi node is considered a declare
getVarDeclBBIds :: M.OrderedMap BBId BasicBlock -> M.OrderedMap (Label Inst) [BBId]
getVarDeclBBIds bbs = M.fromListWith (++) $ do
    (k :: BBId , bb) <- M.toList bbs
    inst <- bbInsts bb
    guard $ isDeclInst_ inst

    return (namedName inst, [k])
    where
        isDeclInst_ :: Named Inst  -> Bool
        isDeclInst_ (Named instname inst) = case inst of
                                                InstAlloc -> True
                                                InstPhi _ -> True
                                                _ -> False
-- | Rename all instructions in a basic block
renameInstsInBB :: Label Inst -- ^Old label
                 -> Label Inst -- ^New label
                 -> BasicBlock -- ^Basic Block to repalce in
                 -> BasicBlock
renameInstsInBB oldl newl (bb@BasicBlock{bbInsts=bbInsts}) = bb {
    bbInsts=map replaceLabel bbInsts
} where
    replaceLabel (Named lbl inst) = let lbl' = if lbl == oldl then newl else lbl
                                    in (Named lbl'  (replaceInstRef_ inst))

    -- | Replace references to label in Inst
    replaceInstRef_ :: Inst -> Inst
    replaceInstRef_ inst = mapInstValue replaceValueInstRef_ inst

    replaceValueInstRef_ :: Value -> Value
    replaceValueInstRef_ (ValueInstRef lbl) =
        let lbl' = if lbl == oldl then newl else lbl in ValueInstRef lbl'
    replaceValueInstRef_ v = v

-- | Rename all instructions in the dom-set of a basic block
renameInstsInDomSet :: Label Inst -- ^Original label
                   -> Label Inst -- ^New label
                   -> BBIdToDomSet -- ^Dominator sets
                   -> BBId -- ^ID to BB to start from
                   -> M.OrderedMap BBId BasicBlock
                   -> M.OrderedMap BBId BasicBlock
renameInstsInDomSet l l' bbIdToDomSet bbid bbmap =
    adjustWithKeys renamer domset bbmap where
        domset :: S.Set BBId
        domset = bbIdToDomSet M.! bbid

        renamer :: BBId -> BasicBlock -> BasicBlock
        renamer _ bb = renameInstsInBB l l' bb


-- | Take a map and create a unique mapping from a key to each value.
-- | Hold the old key in the place where the new key maps to
-- | Take a map and create a unique mapping from a key to each value
uniquifyKeys :: Ord k => (Int -> k -> k) -> M.OrderedMap k [a] -> M.OrderedMap k (k, a)
uniquifyKeys uniqf m = M.fromList $ M.foldMapWithKey (uniqifyPerKey_ uniqf) m where
    uniqifyPerKey_ :: (Int -> k -> k) -> k -> [a] -> [(k, (k, a))]
    uniqifyPerKey_ uniqf k as = foldMap (uniqifyPerKeyVal_ uniqf k) (zip [1..] as)

    uniqifyPerKeyVal_ :: (Int -> k -> k) -> k -> (Int, a) -> [(k, (k, a))]
    uniqifyPerKeyVal_ uniqf k (i, a) =  [(uniqf i k, (k, a))]


data VariableRenameContext = VariableRenameContext {
  -- | Map from every instruction to the number of occurences.
  ctxVarToCount :: M.OrderedMap (Label Inst) Int,
  -- | Map a name to the latest value that was stored in it.
  -- | This is used to collapse load / store in a BB.
  ctxVarToLatestStoreVal :: M.OrderedMap (Label Inst) Value,
  -- | Function
  ctxBBMap :: M.OrderedMap (BBId) BasicBlock
}
instance Pretty VariableRenameContext where
    pretty (VariableRenameContext{..}) =
        vcat [pretty "vartocount:", pretty ctxVarToCount,
              pretty "vartoval:", pretty ctxVarToLatestStoreVal]

-- | Bump up the count of a variable.
bumpUpCount :: Label Inst -> State VariableRenameContext ()
bumpUpCount name = do
  varToCount <- gets ctxVarToCount
  -- If a value exists, bump it up. Otherwise set to 0
  let count' = M.insertWith (\new old -> old + 1) name 0 varToCount
  modify (\ctx -> ctx {ctxVarToCount=count'})

-- | Get the count of a variable.
getVarCount :: Label Inst -> State VariableRenameContext Int
getVarCount name = do
    modify (\ctx -> ctx {ctxVarToCount = M.insertWith (\new old -> old) name 0 (ctxVarToCount ctx)})
    gets (\ctx -> ctxVarToCount ctx M.! name)

getLatestName :: Label Inst -> State VariableRenameContext (Label Inst)
getLatestName name = do
    count <- getVarCount name
    return $ Label $ (unLabel name) ++ ".renamed." ++ show count


-- | Function to rename value based on the current rename counts
-- | If there is a value remapping from a "store", then use that
-- | If there is no remapping, then don't care
renameValue_ :: Value -> State VariableRenameContext Value
renameValue_ v@(ValueConstInt _) = return v
renameValue_ (ValueInstRef name) = do
    varToValue <- gets ctxVarToLatestStoreVal
    case M.lookup name varToValue of
      Just val -> return val
      Nothing -> ValueInstRef <$> getLatestName name


-- | Rename bindings in the RHS of an instruction.
variableRenameInstRHS :: Inst -> State VariableRenameContext Inst
variableRenameInstRHS inst = forInstValue renameValue_  inst

-- | Rename bindings in the LHS of an instruction
variableRenameInstLHS :: Named Inst -> State VariableRenameContext (Named Inst)
variableRenameInstLHS namedInst@(Named name inst) = do
   bumpUpCount name
   curValToCount <- gets ctxVarToCount
   name' <- getLatestName name
   return (Named name' inst)


-- | Rename RHS & LHS of an instruction correctly
-- | Can return [] for instructions to be omitted
variableRenameInst :: Named Inst -> State VariableRenameContext [Named Inst]
variableRenameInst namedinst@(Named name inst) = do
    case inst of
        InstStore (ValueInstRef slot) val -> do
          valNamed <- renameValue_ val
          modify (\ctx -> ctx {ctxVarToLatestStoreVal=M.insert slot valNamed (ctxVarToLatestStoreVal ctx)})
          return []

        InstLoad (ValueInstRef slot) -> do
          loadedval <- renameValue_ (ValueInstRef slot)
          modify (\ctx -> ctx {ctxVarToLatestStoreVal=M.insert name loadedval (ctxVarToLatestStoreVal ctx)})
          return []

        -- | We don't need allocs, so delete them
        InstAlloc -> return []

        _ -> do
          -- | Rename RHS
          (rhsrenamed :: Named Inst) <- forM namedinst variableRenameInstRHS
          lhsrenamed <- variableRenameInstLHS rhsrenamed
          return [lhsrenamed]



-- | Rename the return instruction
variableRenameRetInst :: RetInst -> State VariableRenameContext (RetInst)
variableRenameRetInst ret = forRetInstValue renameValue_ ret

-- | Rename phi nodes in bb
variableRenamePhiNodes :: BBId -- ^Current BB Id
                          -> BBId -- ^Phi node BB Id
                          -> State VariableRenameContext ()
variableRenamePhiNodes curbbid phibbid = do
    varToCount <- gets ctxVarToCount
    varToValue <- gets ctxVarToLatestStoreVal
    phibb <- gets (\ctx -> (ctxBBMap ctx) M.! phibbid)
    phiBBInsts' <- forM (bbInsts phibb) (\(Named name inst) -> (Named name) <$> (phiRenamer curbbid inst))

    let phibb' = phibb {
      bbInsts=phiBBInsts'
    }

    modify (\ctx -> ctx { ctxBBMap= M.insert phibbid phibb' (ctxBBMap ctx) })
    where
    -- | Rename only phi ndoes leaving other instructions
    phiRenamer :: BBId -- ^ Current BB Id
                  -> Inst -- ^Instruction from the child BB
                  -> State VariableRenameContext Inst
    phiRenamer curbbid (InstPhi philist) = InstPhi <$> (renamePhiList curbbid philist)
    phiRenamer _ (inst) = return inst

    -- | Rename bindings in a phi node
    renamePhiList :: BBId -- ^ Current BB Id
                     -> NE.NonEmpty (BBId, Value) -- ^Phi node parameters
                     -> State VariableRenameContext (NE.NonEmpty (BBId, Value))
    renamePhiList curbbid philist =
      forM philist (renamePhiBinding curbbid)
    -- | Rename a single binding in the phi node entry list
    renamePhiBinding :: BBId -> (BBId, Value) -> State VariableRenameContext (BBId, Value)
    renamePhiBinding curbbid (bbid, value) = (bbid,)  <$> stateValue'
        where stateValue' = if bbid == curbbid
                        then renameValue_ value
                        else return value



-- | Rename all instructions and the return instructoin at a given BB
instructionsRenameAtBB :: BBId -> State VariableRenameContext ()
instructionsRenameAtBB curbbid = do
  bbmap <- gets ctxBBMap
  modify (\ctx -> ctx { ctxBBMap= (ctxBBMap ctx) })
  let curbb = bbmap M.! curbbid
  bbInsts' <- mconcat <$> for (bbInsts curbb) variableRenameInst

  bbRetInst' <- variableRenameRetInst (bbRetInst curbb)

  let curbb' = curbb { bbInsts = bbInsts', bbRetInst = bbRetInst' }
  let bbmap' = M.insert curbbid curbb' bbmap

  modify (\ctx -> ctx { ctxBBMap=bbmap' })

-- | Copy back numbering information from the old context to the new context
resetNumbering :: VariableRenameContext -> VariableRenameContext -> VariableRenameContext
resetNumbering oldctx newctx = newctx {
    ctxVarToLatestStoreVal=ctxVarToLatestStoreVal oldctx,
    ctxVarToCount=ctxVarToCount oldctx
}

resetVarMappings :: State VariableRenameContext ()
resetVarMappings = modify (\ctx -> ctx { ctxVarToLatestStoreVal=mempty })

-- 1. rename instructions
-- 2. rename phi nodes of children in CFG
-- 3. follow process into children of dominator tree
-- TODO: fold current BBId into the state or something, this is a HUGE mess.
-- | Like seriously, I hate myself a little for *writing* this bastard.
variableRenameAtBB :: CFG -> DomTree -> BBId -> State VariableRenameContext ()
variableRenameAtBB cfg domtree curbbid = do
  -- | 1. Reset variable mappings from your parent because they will
  -- | No longer hold for the child
  -- | eg.
  -- | Parent(y=1)
  -- |    |      |
  -- |   (y=2)  (y=3)
  -- |     \      /
  -- |      ------
  -- |         |
  -- |        child <- is dominated by parent, CANNOT assume (y = 1)
  -- | Pull out final states to reset these for each child
  resetVarMappings
  parentctx <- get
  -- | Rename all instructions at BB
  instructionsRenameAtBB curbbid

  -- | Rename Phi nodes of children in CFG
  let cfgChildrenIDs = getImmediateChildren cfg curbbid
  forM_ cfgChildrenIDs (variableRenamePhiNodes curbbid)


  let domTreeChildrenIDs = getImmediateChildren domtree curbbid
  forM_ domTreeChildrenIDs (\childid -> do

                                  varToVal <- gets ctxVarToLatestStoreVal
                                  variableRenameAtBB cfg domtree childid)
  -- | reset these states across children
  modify (resetNumbering parentctx)
  return ()




-- | Rename variables to be unique in the function.
variableRename_ :: CFG  -- ^ The CFG for the program.
                   -> DomTree -- ^Dominator tree for the program.
                   -> BBId -- ^Entry BBId
                   -> M.OrderedMap BBId BasicBlock -- ^Function
                   -> M.OrderedMap BBId BasicBlock
variableRename_ cfg domtree entrybbid bbmap =
    ctxBBMap $ execState (variableRenameAtBB cfg domtree entrybbid) initctx where
      initctx :: VariableRenameContext
      initctx = VariableRenameContext { ctxVarToCount=mempty,
                                        ctxBBMap=bbmap,
                                        ctxVarToLatestStoreVal=mempty
                                      }


transformMem2Reg :: IRProgram -> IRProgram
transformMem2Reg program@IRProgram{irProgramBBMap=bbmap,
                           irProgramEntryBBId=entrybbid} =
    (IRProgram {irProgramBBMap=bbmap', irProgramEntryBBId=entrybbid}) where
      cfg :: CFG
      cfg =  mkBBGraph bbmap

      bbIdToDomSet :: BBIdToDomSet
      bbIdToDomSet = constructBBDominators program

      domtree :: DomTree
      domtree = constructDominatorTree bbIdToDomSet entrybbid

      bbmapWithPhi :: M.OrderedMap BBId BasicBlock
      bbmapWithPhi = (placePhiNodes_ cfg domtree) $ bbmap

      bbmap' = (variableRename_ cfg domtree entrybbid) bbmapWithPhi
