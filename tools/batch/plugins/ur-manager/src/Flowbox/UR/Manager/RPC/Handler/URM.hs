---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Flowbox.UR.Manager.RPC.Handler.URM where

import           Control.Monad.Trans.State.Lazy                      (get, put)
import qualified Data.DList                                          as DList
import qualified Data.Map                                            as Map
import           Data.Maybe                                          (fromMaybe)

import           Flowbox.Bus.Data.Message                            (Message)
import           Flowbox.Bus.Data.Serialize.Proto.Conversion.Message ()
import           Flowbox.Bus.RPC.RPC                                 (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                     hiding (Context, error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                        as Proto
import           Flowbox.UR.Manager.Context                          (Context, ProjectContext(..), Stack)
import qualified Flowbox.UR.Manager.Context                          as Context
import qualified Generated.Proto.Urm.URM.ClearStack.Request          as ClearStack
import qualified Generated.Proto.Urm.URM.ClearStack.Status           as ClearStack
import qualified Generated.Proto.Urm.URM.Redo.Descriptions.Request   as RDescriptions
import qualified Generated.Proto.Urm.URM.Redo.Descriptions.Status    as RDescriptions
import qualified Generated.Proto.Urm.URM.Redo.Request                as Redo
import qualified Generated.Proto.Urm.URM.Redo.Status                 as Redo
import qualified Generated.Proto.Urm.URM.Register.Request            as Register
import qualified Generated.Proto.Urm.URM.Register.Status             as Register
import qualified Generated.Proto.Urm.URM.RegisterMultiple.Request    as RegisterMultiple
import qualified Generated.Proto.Urm.URM.RegisterMultiple.Status     as RegisterMultiple
import qualified Generated.Proto.Urm.URM.Transaction.Begin.Request   as TBegin
import qualified Generated.Proto.Urm.URM.Transaction.Begin.Status    as TBegin
import qualified Generated.Proto.Urm.URM.Transaction.Commit.Request  as TCommit
import qualified Generated.Proto.Urm.URM.Transaction.Commit.Status   as TCommit
import qualified Generated.Proto.Urm.URM.Undo.Descriptions.Request   as UDescriptions
import qualified Generated.Proto.Urm.URM.Undo.Descriptions.Status    as UDescriptions
import qualified Generated.Proto.Urm.URM.Undo.Request                as Undo
import qualified Generated.Proto.Urm.URM.Undo.Status                 as Undo


logger :: LoggerIO
logger = getLoggerIO $(moduleName)


register :: Register.Request -> RPC Context IO Register.Status
register request@(Register.Request undoAction redoAction tprojectID tdescription) = do
    let projectID = decodeP tprojectID
    let description = decodeP tdescription
    reg projectID [decodeP undoAction] (decodeP redoAction) description
    return $ Register.Status request True


registerMultiple :: RegisterMultiple.Request -> RPC Context IO RegisterMultiple.Status
registerMultiple request@(RegisterMultiple.Request undoActions redoAction tprojectID tdescription) = do
    let projectID = decodeP tprojectID
    let description = decodeP tdescription
    reg projectID (decodeP undoActions) (decodeP redoAction) description
    return $ RegisterMultiple.Status request True

reg :: Int -> [Message] -> Message -> String -> RPC Context IO ()
reg projectID undoA redoA description = do
    let transaction d = (DList.singleton (undoA, redoA), d)
    contextMap <- lift get
    pContext@(ProjectContext undoL _ maybeTr) <- return $ fromMaybe (Context.emptyProjectContext) $ Map.lookup projectID contextMap
    let newContext = case maybeTr of
                        Just tr ->
                            let newTransaction = Just $ _1 %~ (`DList.snoc` (undoA, redoA)) $ tr
                            in  Context.trans .~ newTransaction $ pContext
                        Nothing -> ProjectContext (transaction description : undoL) [] Nothing
    logger warning (case maybeTr of
                        Just tr -> "appended transaction: " ++ (snd tr) ++ " with " ++ description
                        Nothing -> "added action: " ++ description)
    lift $ put $ Map.insert projectID newContext contextMap


undo :: Undo.Request -> RPC Context IO (Undo.Status, Maybe [Message])
undo request@(Undo.Request tprojectID) = execAction (decodeP tprojectID) fst            id   reverse $ Undo.Status request

redo :: Redo.Request -> RPC Context IO (Redo.Status, Maybe [Message])
redo request@(Redo.Request tprojectID) = execAction (decodeP tprojectID) (return . snd) flip id      $ Redo.Status request 

execAction :: Proto.Serializable ret =>
                     -- could be ((a, b) -> c) but "c ~ Message could not be deduced"
              Int -> (Context.Actions -> [Message]) -> ((Stack -> Stack -> (Maybe Context.Transaction -> ProjectContext)) -> Stack -> Stack -> (Maybe Context.Transaction -> ProjectContext)) ->
              ([Context.Actions] -> [Context.Actions]) -> (Bool-> ret) -> RPC Context IO (ret, Maybe [Message])
execAction projectID accessor invert rev retCons = do
    contextMap <- lift get
    let projContexts = Map.lookup projectID contextMap 
        ProjectContext stack1 stack2 trans = maybe (Context.emptyProjectContext)
                                                   (\(ProjectContext a b t) -> invert ProjectContext a b $ t)
                                                   projContexts
    case stack1 of
        [] -> 
            return (retCons False, Nothing)
        (action : rest) -> do 
            let newMap = invert ProjectContext rest (action : stack2) $ trans
            lift $ put $ Map.insert projectID newMap contextMap
            logger warning $ "Undo/Redo on " ++ (snd action)
            return (retCons True, Just $ (rev $ DList.toList $ fst action) >>= accessor)


clearStack :: ClearStack.Request -> RPC Context IO ClearStack.Status
clearStack request@(ClearStack.Request tprojectID) = do
    let projectID = decodeP tprojectID
    lift . put . Map.insert projectID Context.emptyProjectContext =<< lift get
    logger warning "clearing stack"
    return $ ClearStack.Status request 


-- [MW] TODO: nested transactions
tBegin :: TBegin.Request -> RPC Context IO TBegin.Status
tBegin request@(TBegin.Request tprojectID tdescription) = do
    let projectID   = decodeP tprojectID
        description = decodeP tdescription
        createTrans = Context.trans .~ Just (DList.empty, description)
    lift . put . (Map.adjust createTrans projectID) =<< lift get
    logger warning $ "opening transaction: " ++ description
    return $ TBegin.Status request

tCommit :: TCommit.Request -> RPC Context IO TCommit.Status
tCommit request@(TCommit.Request tprojectID) = do
   let projectID = decodeP tprojectID 
   context <- lift get
   let pContext@(ProjectContext undo redo trans) = fromMaybe Context.emptyProjectContext $ Map.lookup projectID context
       newUndo = maybe undo (: undo) trans
   logger warning $ "projID" ++ (show projectID)
   lift $ put $ Map.insert projectID (ProjectContext newUndo [] Nothing) context
   logger warning $ "closing transaction: " ++ (snd $ head newUndo)
   return $ TCommit.Status request


undoDescriptions :: UDescriptions.Request -> RPC Context IO UDescriptions.Status
undoDescriptions request@(UDescriptions.Request tprojectID) = do
    let projectID = decodeP tprojectID
    oko <- lift get
    let context = Map.lookup projectID oko
    return $ (UDescriptions.Status request) $ maybe (encodeP [""]) (encodeP . map snd . (^. Context.undo)) context
                                                                                                                                
redoDescriptions :: RDescriptions.Request -> RPC Context IO RDescriptions.Status                                                
redoDescriptions request@(RDescriptions.Request tprojectID) = do
    let projectID = decodeP tprojectID
    oko <- lift get
    let context = Map.lookup projectID oko
    return $ (RDescriptions.Status request) $ maybe (encodeP [""]) (encodeP . map snd . (^. Context.redo)) context


--descriptions :: lens -> Int -> (descriptions -> response) -> RPC Context IO response
--descriptions lens projectID responseCons = do
--    oko <- lift get
--    let context = Map.lookup projectID oko
--    return $ responseCons $ maybe [] (map snd . (^. lens)) context
--
--undoDescriptions :: UDescriptions.Request -> RPC Context IO UDescriptions.Status
--undoDescriptions request@(UDescriptions.Request tprojectID) = descriptions Context.undo (encodeP tprojectID) (UDescriptions.Status request)
--                                                                                                                                
--redoDescriptions :: RDescriptions.Request -> RPC Context IO RDescriptions.Status                                                
--redoDescriptions request@(RDescriptions.Request tprojectID) = descriptions Context.redo (encodeP tprojectID) (RDescriptions.Status request)
--
--descriptions :: lens -> Int -> (descriptions -> response) -> RPC Context IO response
--descriptions lens projectID responseCons = do
--    oko <- lift get
--    let context = Map.lookup projectID oko
--    return $ responseCons $ maybe [] (map snd . (^. lens)) context
