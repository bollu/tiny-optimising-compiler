<h1> Memory to Register, or, how to get to SSA <h1>

<h2> Equivalent LLVM passes </h2>

- [Mem2Reg](https://llvm.org/docs/Passes.html#mem2reg-promote-memory-to-register)
- [SROA](https://llvm.org/docs/Passes.html#sroa-scalar-replacement-of-aggregates)

SROA is strictly more powerful that `mem2reg`, since it can break up arrays and
structures into SSA form. However, the core algorithm remains the same.

<h2> Introduction </h2>



\begin{code}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TransformMem2Reg(constructDominatorTree,
    CFG(..),
    mkCFG,
    constructBBDominators,
    getAllChildren,
    getDominanceFrontier,
    DomTree,
    BBIdToDomSet,
    transformMem2Reg) where

import IR
import BaseIR
import Data.Tree
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
import Graph

-- | The control flow graph, which is a graph of basic blocks
type CFG = Graph IRBBId


-- | Get the successors of this basic block
getBBSuccessors :: IRBB -> [IRBBId]
getBBSuccessors (BasicBlock { bbRetInst = RetInstTerminal}) = []
getBBSuccessors (BasicBlock { bbRetInst = RetInstRet _}) = []
getBBSuccessors (BasicBlock { bbRetInst = RetInstBranch next}) = [next]
getBBSuccessors (BasicBlock { bbRetInst = RetInstConditionalBranch _ l r}) = [l, r]

mkCFG :: M.OrderedMap IRBBId IRBB -> CFG
mkCFG bbMap = Graph (M.foldMapWithKey makeEdges bbMap)  where

    -- Make the edges corresponding to basic block.
    makeEdges :: IRBBId -> IRBB -> [(IRBBId, IRBBId)]
    makeEdges bbid bb = map (\succ -> (bbid, succ)) (getBBSuccessors bb)

-- a dominator tree is a tree of basic blocks
newtype DominatorTree  = Tree IRBB

-- BBId of the root node.
type EntryBBId = IRBBId


-- | Set of nodes that dominate a node.
type DomSet =  S.Set IRBBId

instance Pretty a => Pretty (S.Set a) where
  pretty = pretty . S.toList

-- | Map from a node to the set of nodes that dominate it
type BBIdToDomSet = M.OrderedMap IRBBId DomSet


initialBBIdToDomSet :: EntryBBId ->  -- ^entry BB ID
                  [IRBBId] ->  -- ^All BB IDs
                  BBIdToDomSet
initialBBIdToDomSet entryid ids = M.fromList (mapEntry:mapAllExceptEntry) where
  -- entry block only dominantes itself
  mapEntry :: (IRBBId, DomSet)
  mapEntry = (entryid, S.fromList [entryid])
  -- IDs other than the entry block
  nonEntryIds :: [IRBBId]
  nonEntryIds = filter (/= entryid) ids
  -- list mapping basic block Ids to dominance sets
  mapAllExceptEntry ::  [(IRBBId, DomSet)]
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
  computeNewDom :: IRBBId -> DomSet -> DomSet
  computeNewDom id old = if id == entryid then old else computeNewNonRootDom id
  -- compute the dom set of a node that is not the root
  computeNewNonRootDom :: IRBBId -> DomSet
  computeNewNonRootDom bbid = (combinePredDomSets ((getDoms . preds) bbid)) `S.union` (S.singleton bbid)

  -- predecessors of id
  preds :: IRBBId -> [IRBBId]
  preds bbid = getPredecessors cfg bbid

  -- combine all predecessor dom sets by intersecting them
  combinePredDomSets :: [DomSet] -> DomSet
  combinePredDomSets [] = error "unreachable node in domset"
  combinePredDomSets ds = foldl1 S.intersection ds

  -- get dominators of ids
  getDoms :: [IRBBId] -> [DomSet]
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
    cfg =  mkCFG (programBBMap program)

    -- seed constructBBDominators
    initdominfo :: BBIdToDomSet
    initdominfo = initialBBIdToDomSet entryid bbids

    -- ID of the root node
    entryid :: IRBBId
    entryid = programEntryBBId program

    -- list of all basic block IDs
    bbids :: [IRBBId]
    bbids = M.keys (programBBMap program)


data DomTreeContext = DomTreeContext {
  ctxBBIdToDomSet :: BBIdToDomSet,
  ctxEntryId :: EntryBBId
}

-- | Returns the dominators of BBId
getBBDominators :: IRBBId -> Reader DomTreeContext DomSet
getBBDominators bbid = do
  bbIdToDomSet <- reader ctxBBIdToDomSet
  return $ bbIdToDomSet M.! bbid

-- | Returns the struct dominators of BBId
getBBStrictDominators :: IRBBId -> Reader DomTreeContext DomSet
getBBStrictDominators bbid = S.filter (/= bbid) <$> (getBBDominators bbid)


-- | Returns whether y dominates x
doesDominate :: IRBBId -> IRBBId -> Reader DomTreeContext Bool
doesDominate x y = do
    bbIdToDomSet <- reader ctxBBIdToDomSet
    return $ x `S.member` (bbIdToDomSet M.! y)

-- | Run a forall in a traversable for a monadic context
allTraversable :: (Foldable t, Traversable t, Monad m) => t a -> (a -> m Bool) -> m Bool
allTraversable ta mpred = (foldl (&&) True) <$> (forM ta mpred)

-- | Return if the BBId is dominated by all bbs *other than itself* in others
isDominatedByAllOthers :: [IRBBId] -> IRBBId -> Reader DomTreeContext Bool
isDominatedByAllOthers others self =
  allTraversable others(\other -> if other == self then return True
                                                    else doesDominate other self)

-- | Returns the immediate dominator if present, otherwise returns Nothing
getImmediateDominator :: IRBBId -> Reader DomTreeContext (Maybe IRBBId)
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
type DomTree = Graph IRBBId

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
constructDominatorTree :: M.OrderedMap IRBBId DomSet -> EntryBBId -> DomTree
constructDominatorTree bbidToDomSet entrybb  = runReader createDominatorTree_ (DomTreeContext bbidToDomSet entrybb)




-- | The `dominates` relation. Given two BB Ids, returns whether `a` dominates `b`
dominates :: DomTree -> IRBBId -> IRBBId -> Bool
dominates tree a b = b `elem` (getAllChildren tree a)

-- | Strictly dominates relation. `A` strictlyDom `B` iff `A` Dom `B` and `A` /= `B`
strictlyDominates :: DomTree -> IRBBId -> IRBBId -> Bool
strictlyDominates domtree a b = dominates domtree a b && a /= b

-- | Get the list of dominance frontiers of a given BB
-- | "The dominance frontier of a node d is the set of all nodes n such that d
-- |  dominates an immediate predecessor of n, but d does not strictly dominate
-- |  n. It is the set of nodes where d's dominance stops."
-- | TODO: think anbout why *an immediate preceseeor*, not *all immediate ...*.
getDominanceFrontier :: DomTree -> CFG -> IRBBId -> [IRBBId]
getDominanceFrontier tree@(Graph domedges) cfg cur =
  [bb | bb <- vertices tree, any (dominates tree cur) (preds bb) , not (strictlyDominates tree cur bb)] where
  preds bb = (getPredecessors cfg bb)


-- Get the names of all values allocated in a basic block
getBBVarUses :: IRBB -> [Label Inst]
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
insertPhiNodeCallback_ :: CFG -> Label Inst -> IRBBId -> IRBB -> IRBB
insertPhiNodeCallback_ cfg lbl bbid bb@(BasicBlock{..}) =
    bb {bbInsts=bbInsts'} where
    bbInsts' :: [Named Inst]
    bbInsts' = (lbl =:= phi):bbInsts

    phi :: Inst
    phi = InstPhi . unsafeToNonEmpty $ (zip ((getPredecessors cfg bbid)) (repeat (ValueInstRef lbl)))


-- | Place Phi nodes for a given instruction at a set of start CFGs. Initially, they should be the
-- |  set of nodes that store to the original value
placePhiNodesForAlloc_ :: Label Inst -- ^Name of the original value
                          -> S.Set IRBBId -- ^BBs to process
                          -> S.Set IRBBId -- ^BBs that are already processed
                          -> CFG -- ^The CFG of the function
                          -> DomTree -- ^The dominator tree of the function
                          -> M.OrderedMap IRBBId IRBB -- ^Function body
                          -> M.OrderedMap IRBBId IRBB
placePhiNodesForAlloc_ name curbbs processed cfg domtree bbmap =
    if null (curbbs)
    then bbmap
    else (placePhiNodesForAlloc_ name curbbs' processed' cfg domtree bbmap')  where
                cur :: IRBBId
                cur = S.elemAt 0 curbbs

                -- | For every basic block in the dominance frontier, insert a phi node.
                bbmap' :: M.OrderedMap IRBBId IRBB
                bbmap' = adjustWithKeys (insertPhiNodeCallback_ cfg name) curfrontier bbmap

                curbbs' :: S.Set IRBBId
                curbbs' = (curbbs `S.union` curfrontier) S.\\ processed'

                curfrontier ::  S.Set IRBBId
                curfrontier = (S.fromList $ getDominanceFrontier domtree cfg cur) S.\\ processed'

                processed' :: S.Set IRBBId
                processed' = (S.insert cur processed)

                debugStr :: String
                debugStr = docToString $ pretty "running for: " <+> pretty name <+> pretty "curbbs: " <+> pretty curbbs <+> pretty "cur: " <+> pretty cur <+> pretty "frontiner: " <+> pretty curfrontier

mapReverse :: (Pretty k, Pretty a, Ord k, Ord a) => M.OrderedMap k [a] -> M.OrderedMap a [k]
mapReverse m = M.fromListWith (++) [(a, [k]) | (k, as) <- M.toList m, a <- as]

-- References: http://www.cs.is.noda.tus.ac.jp/~mune/keio/m/ssa2.pdf
placePhiNodes_ :: CFG -> DomTree -> M.OrderedMap IRBBId IRBB -> M.OrderedMap IRBBId IRBB
placePhiNodes_ cfg domtree initbbmap =
    M.foldlWithKey (\curbbmap name bbids -> placePhiNodesForAlloc_ name (S.fromList bbids) mempty cfg domtree curbbmap) initbbmap usesToBBIds  where
      bbIdToUses :: M.OrderedMap IRBBId [Label Inst]
      bbIdToUses = fmap getBBVarUses initbbmap

      usesToBBIds :: M.OrderedMap (Label Inst) [IRBBId]
      usesToBBIds = mapReverse bbIdToUses

-- | Find wherever this variable is "declared". An Alloca or a Phi node is considered a declare
getVarDeclBBIds :: M.OrderedMap IRBBId IRBB -> M.OrderedMap (Label Inst) [IRBBId]
getVarDeclBBIds bbs = M.fromListWith (++) $ do
    (k :: IRBBId , bb) <- M.toList bbs
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
                 -> IRBB -- ^Basic Block to replace in
                 -> IRBB
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
                   -> IRBBId -- ^ID to BB to start from
                   -> M.OrderedMap IRBBId IRBB
                   -> M.OrderedMap IRBBId IRBB
renameInstsInDomSet l l' bbIdToDomSet bbid bbmap =
    adjustWithKeys renamer domset bbmap where
        domset :: S.Set IRBBId
        domset = bbIdToDomSet M.! bbid

        renamer :: IRBBId -> IRBB -> IRBB
        renamer _ bb = renameInstsInBB l l' bb


-- -- | Take a map and create a unique mapping from a key to each value.
-- -- | Hold the old key in the place where the new key maps to
-- -- | Take a map and create a unique mapping from a key to each value
-- uniquifyKeys :: Ord k => (Int -> k -> k) -> M.OrderedMap k [a] -> M.OrderedMap k (k, a)
-- uniquifyKeys uniqf m = M.fromList $ M.foldMapWithKey (uniqifyPerKey_ uniqf) m where
--     uniqifyPerKey_ :: (Int -> k -> k) -> k -> [a] -> [(k, (k, a))]
--     uniqifyPerKey_ uniqf k as = foldMap (uniqifyPerKeyVal_ uniqf k) (zip [1..] as)
--
--     uniqifyPerKeyVal_ :: (Int -> k -> k) -> k -> (Int, a) -> [(k, (k, a))]
--     uniqifyPerKeyVal_ uniqf k (i, a) =  [(uniqf i k, (k, a))]


data VariableRenameContext = VariableRenameContext {
  -- | Map a name to the latest value that was stored in it.
  -- | This is used to collapse load / store in a BB.
  ctxVarToLatestStoreVal :: M.OrderedMap (Label Inst) Value,
  -- | Function
  ctxBBMap :: M.OrderedMap (IRBBId) IRBB
}
instance Pretty VariableRenameContext where
    pretty (VariableRenameContext{..}) =
        vcat [pretty "vartoval:", pretty ctxVarToLatestStoreVal]

-- -- | Bump up the count of a variable.
-- bumpUpCount :: Label Inst -> State VariableRenameContext ()
-- bumpUpCount name = do
--   varToCount <- gets ctxVarToCount
--   -- If a value exists, bump it up. Otherwise set to 0
--   let count' = M.insertWith (\new old -> old + 1) name 0 varToCount
--   modify (\ctx -> ctx {ctxVarToCount=count'})
--
-- -- | Get the count of a variable.
-- getVarCount :: Label Inst -> State VariableRenameContext Int
-- getVarCount name = do
--     modify (\ctx -> ctx {ctxVarToCount = M.insertWith (\new old -> old) name 0 (ctxVarToCount ctx)})
--     count <- gets (\ctx -> case M.lookup name (ctxVarToCount ctx) of
--                                 Just c -> c
--                                 Nothing -> error . docToString $ pretty "getVarCount, unknown name:" <+>  pretty name)
--     return count

-- getLatestName :: Label Inst -> State VariableRenameContext (Label Inst)
-- getLatestName name = do
--     count <- getVarCount name
--     return $ Label $ (unLabel name) ++ ".renamed." ++ show count
--

-- | Function to rename value based on the current rename counts
-- | If there is a value remapping from a "store", then use that
-- | If there is no remapping, then don't care
-- renameValue_ :: Value -> State VariableRenameContext Value
-- renameValue_ v@(ValueConstInt _) = return v
-- renameValue_ (ValueInstRef name) = do
--     varToValue <- gets ctxVarToLatestStoreVal
--     case M.lookup name varToValue of
--       Just val -> return val
--       Nothing -> ValueInstRef <$> getLatestName name
--
-- NOTE: this is a hack. The correct thing to do is to replace everything in the
-- BB that refers to this value.
renameStoredValue_ :: Value -> State VariableRenameContext Value
renameStoredValue_ v@(ValueConstInt _) = return v
renameStoredValue_ orig@(ValueInstRef name) = do
    varToValue <- gets ctxVarToLatestStoreVal
    case M.lookup name varToValue of
      Just val -> do
                    if orig == val
                    then return val
                    else renameStoredValue_ val
      Nothing -> return orig

getLatestStoredValue_ :: Label Inst -> State VariableRenameContext Value
getLatestStoredValue_ lbl = gets (\ctx -> (ctxVarToLatestStoreVal ctx) M.! lbl)

-- | Rename bindings in the RHS of an instruction.
variableRenameInstRHS :: Inst -> State VariableRenameContext Inst
variableRenameInstRHS inst = forInstValue renameStoredValue_  inst

-- | Rename bindings in the LHS of an instruction
-- variableRenameInstLHS :: Named Inst -> State VariableRenameContext (Named Inst)
-- variableRenameInstLHS namedInst@(Named name inst) = do
--    bumpUpCount name
--    curValToCount <- gets ctxVarToCount
--    name' <- getLatestName name
--    return (Named name' inst)


-- | Rename RHS & LHS of an instruction correctly
-- | Can return [] for instructions to be omitted
variableRenameInst :: Named Inst -> State VariableRenameContext [Named Inst]
variableRenameInst namedinst@(Named name inst) = do
    case inst of
        InstStore (ValueInstRef slot) val -> do
          valNamed <- renameStoredValue_ val
          modify (\ctx -> ctx {ctxVarToLatestStoreVal=M.insert slot valNamed (ctxVarToLatestStoreVal ctx)})
          return []

        InstLoad (ValueInstRef slot) -> do
          loadedval <- getLatestStoredValue_ slot
          modify (\ctx -> ctx {ctxVarToLatestStoreVal=M.insert name loadedval (ctxVarToLatestStoreVal ctx)})
          return []

        -- | For a phi node, all predecessors should map to phi node name.
        -- | We have already rewritten the values incoming to the phi node,
        -- | So all we need to do is fix preds.
        InstPhi namebbpairs -> do
            for namebbpairs (\(prevname, prevbbid) ->
                                modify (\ctx -> (ctx {ctxVarToLatestStoreVal=M.insert name  (ValueInstRef name) (ctxVarToLatestStoreVal ctx)})))
            return [namedinst]



        -- | We don't need allocs, so delete them
        InstAlloc -> return []

        _ -> do
          -- | Rename RHS
          (rhsrenamed :: Named Inst) <- forM namedinst variableRenameInstRHS
          -- lhsrenamed <- variableRenameInstLHS rhsrenamed
          return [rhsrenamed]



-- | Rename the return instruction
variableRenameRetInst :: RetInst -> State VariableRenameContext (RetInst)
variableRenameRetInst ret = forRetInstValue renameStoredValue_ ret

-- | Rename phi nodes in bb
variableRenamePhiNodes :: IRBBId -- ^Current BB Id
                          -> IRBBId -- ^Phi node BB Id
                          -> State VariableRenameContext ()
variableRenamePhiNodes curbbid phibbid = do
    varToValue <- gets ctxVarToLatestStoreVal
    phibb <- gets (\ctx -> (ctxBBMap ctx) M.! phibbid)
    phiBBInsts' <- forM (bbInsts phibb) (\(Named name inst) -> (Named name) <$> (phiRenamer curbbid inst))

    let phibb' = phibb {
      bbInsts=phiBBInsts'
    }

    modify (\ctx -> ctx { ctxBBMap= M.insert phibbid phibb' (ctxBBMap ctx) })
    where
    -- | Rename only phi ndoes leaving other instructions
    phiRenamer :: IRBBId -- ^ Current BB Id
                  -> Inst -- ^Instruction from the child BB
                  -> State VariableRenameContext Inst
    phiRenamer curbbid (InstPhi philist) = InstPhi <$> (renamePhiList curbbid philist)
    phiRenamer _ (inst) = return inst

    -- | Rename bindings in a phi node
    renamePhiList :: IRBBId -- ^ Current BB Id
                     -> NE.NonEmpty (IRBBId, Value) -- ^Phi node parameters
                     -> State VariableRenameContext (NE.NonEmpty (IRBBId, Value))
    renamePhiList curbbid philist =
      forM philist (renamePhiBinding curbbid)
    -- | Rename a single binding in the phi node entry list
    renamePhiBinding :: IRBBId -> (IRBBId, Value) -> State VariableRenameContext (IRBBId, Value)
    renamePhiBinding curbbid (bbid, value) = (bbid,)  <$> stateValue'
        where stateValue' = if bbid == curbbid
                        then renameStoredValue_ value
                        else return value


resetVarMappings :: VariableRenameContext -> State VariableRenameContext ()
resetVarMappings parentctx = modify (\ctx -> ctx { ctxVarToLatestStoreVal=ctxVarToLatestStoreVal parentctx })

-- | Rename all instructions and the return instructoin at a given BB
instructionsRenameAtBB :: IRBBId -> State VariableRenameContext ()
instructionsRenameAtBB curbbid = do
  bbmap <- gets ctxBBMap
  modify (\ctx -> ctx { ctxBBMap= (ctxBBMap ctx) })
  let curbb = bbmap M.! curbbid
  bbInsts' <- mconcat <$> for (bbInsts curbb) variableRenameInst

  bbRetInst' <- variableRenameRetInst (bbRetInst curbb)

  let curbb' = curbb { bbInsts = bbInsts', bbRetInst = bbRetInst' }
  let bbmap' = M.insert curbbid curbb' bbmap

  modify (\ctx -> ctx { ctxBBMap=bbmap' })


-- 1. rename instructions
-- 2. rename phi nodes of children in CFG
-- 3. follow process into children of dominator tree
-- TODO: fold current BBId into the state or something, this is a HUGE mess.
-- | Like seriously, I hate myself a little for *writing* this bastard.
variableRenameAtBB :: CFG -> DomTree -> IRBBId -> State VariableRenameContext ()
variableRenameAtBB cfg domtree curbbid = do
  parentctx <- get
  -- | Rename all instructions at BB
  instructionsRenameAtBB curbbid

  -- | Rename Phi nodes of children in CFG
  let cfgChildrenIDs = getImmediateChildren cfg curbbid
  forM_ cfgChildrenIDs (variableRenamePhiNodes curbbid)


  let domTreeChildrenIDs = getImmediateChildren domtree curbbid
  forM_ domTreeChildrenIDs (\childid -> do
                                  variableRenameAtBB cfg domtree childid)
  resetVarMappings parentctx
  return ()




-- | Rename variables to be unique in the function.
lowerMemToReg :: CFG  -- ^ The CFG for the program.
                   -> DomTree -- ^Dominator tree for the program.
                   -> IRBBId -- ^Entry BBId
                   -> M.OrderedMap IRBBId IRBB -- ^Function
                   -> M.OrderedMap IRBBId IRBB
lowerMemToReg cfg domtree entrybbid bbmap =
    ctxBBMap $ execState (variableRenameAtBB cfg domtree entrybbid) initctx where
      initctx :: VariableRenameContext
      initctx = VariableRenameContext { ctxBBMap=bbmap,
                                        ctxVarToLatestStoreVal=mempty
                                      }


transformMem2Reg :: IRProgram -> IRProgram
transformMem2Reg program@Program{programBBMap=bbmap,
                           programEntryBBId=entrybbid} =
    (Program {programBBMap=bbmapReg, programEntryBBId=entrybbid}) where
      cfg :: CFG
      cfg =  mkCFG bbmap

      bbIdToDomSet :: BBIdToDomSet
      bbIdToDomSet = constructBBDominators program

      domtree :: DomTree
      domtree = constructDominatorTree bbIdToDomSet entrybbid

      bbmapWithPhi :: M.OrderedMap IRBBId IRBB
      bbmapWithPhi = (placePhiNodes_ cfg domtree) $ bbmap

      bbmapReg = (lowerMemToReg cfg domtree entrybbid) bbmapWithPhi

\end{code}
