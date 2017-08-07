{-# LANGUAGE RecordWildCards #-}
module IRInterpreter(runProgram) where
import qualified OrderedMap as M
import Control.Monad.State.Strict
import Data.Traversable
import Data.Foldable
import Control.Applicative
import qualified Data.List.NonEmpty as NE
import IR
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils


data Evaluator = Evaluator {
    program :: IRProgram,
    prevbbid :: Maybe BBId,
    valueMap :: M.OrderedMap (Label Inst) Int,
    returnval :: Maybe Int
}
instance Pretty Evaluator where
    pretty Evaluator{..} =pretty "Evaluator" <+> (braces . indent 2) (vcat [pretty "program: ",
                     indent 2 . pretty $ program,
                     pretty "prevbb: ",
                     indent 2 . pretty $ prevbbid,
                     pretty "valueMap: ",
                     indent 2 . pretty $ valueMap,
                     pretty "returnval: ",
                     indent 2 . pretty $ returnval])

initEvaluator :: IRProgram -> Evaluator
initEvaluator program = Evaluator {
    program = program,
    prevbbid = Nothing,
    valueMap = mempty,
    returnval = Nothing
}

loadName :: Label Inst -> State Evaluator Int
loadName name = gets $ (lookupName name) where
    lookupName :: Label Inst -> Evaluator -> Int
    lookupName name evaluator@Evaluator{valueMap=vmap} = case M.lookup name vmap of
                                Just val -> val
                                Nothing -> error . docToString $
                                            vcat [pretty "unable to find mapping to variable:" <+> pretty name,
                                                  pretty "state: ",
                                                  pretty evaluator]


setValue :: Label Inst -> Int -> State Evaluator ()
setValue name val =
    modify (\ctx -> ctx { valueMap=M.insert name val (valueMap ctx) })

getValue :: Value -> State Evaluator Int
getValue (ValueConstInt i) = return i
getValue (ValueInstRef name) = loadName name

getPreviousBBId :: State Evaluator BBId
getPreviousBBId = do
    prevbb <- gets prevbbid
    case prevbb of
        Just id' -> return id'
        Nothing -> error "no previous BB id found."

evaluateInst :: Named Inst -> State Evaluator ()
evaluateInst namedinst@(Named lhsname inst) = do
    case inst of
        InstAlloc -> return ()
        InstLoad slot -> getValue slot >>= setValue lhsname
        InstStore (ValueInstRef slotname) val -> getValue val >>= setValue slotname
        InstAdd l r -> liftA2 (+) (getValue l) (getValue r) >>= setValue lhsname
        InstMul l r -> liftA2 (*) (getValue l) (getValue r) >>= setValue lhsname
        InstL l r -> liftA2 (\l r -> if l < r then 1 else 0) (getValue l) (getValue r) >>= setValue lhsname
        InstAnd l r -> liftA2 (\l r -> l * r) (getValue l) (getValue r) >>= setValue lhsname
        InstPhi bbidValuePairs -> do
                prevbbid <- getPreviousBBId
                getValue (snd (getCurrentBBIdValue prevbbid))  >>= setValue lhsname
            where
                pred :: BBId -> (BBId, Value) -> Bool
                pred prevbbid (bbid, _) = bbid == prevbbid

                getCurrentBBIdValue :: BBId -> (BBId, Value)
                getCurrentBBIdValue prevbbid = head . NE.filter (pred prevbbid) $ bbidValuePairs


followRetInst :: RetInst -> State Evaluator (Maybe BBId)
followRetInst (RetInstTerminal) = return Nothing
followRetInst (RetInstBranch bbid) = return (Just bbid)
followRetInst (RetInstConditionalBranch val t e) = do
    valInt <- getValue val
    if valInt == 1
    then return (Just t)
    else return (Just e)
followRetInst (RetInstRet retval) = do
    retvalInt <- getValue retval
    modify (\evaluator -> evaluator { returnval=Just retvalInt})
    return Nothing

evaluateBB :: BBId -> State Evaluator ()
evaluateBB bbid = do
    bb <- gets $ (M.! bbid) . irProgramBBMap . program
    for (bbInsts bb) evaluateInst
    nextid <- followRetInst (bbRetInst bb)
    modify (\evaluator -> evaluator {prevbbid=Just bbid})

    case nextid of
        Nothing -> return ()
        Just nextid -> evaluateBB nextid

runProgram :: IRProgram -> Maybe Int
runProgram program = returnval $ execState (evaluateBB (irProgramEntryBBId program)) (initEvaluator program)
