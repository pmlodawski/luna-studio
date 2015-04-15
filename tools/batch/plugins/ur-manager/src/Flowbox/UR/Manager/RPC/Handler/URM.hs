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

import           Control.Monad.Trans.State                           (get, put)
import qualified Data.Map                                            as Map
import qualified Data.Set                                            as Set
import           Data.Maybe                                          (fromMaybe, listToMaybe)

import           Flowbox.Prelude                                     hiding (Context, error)

import           Flowbox.Bus.Data.Message                            (CorrelationID, Message)
import qualified Flowbox.Bus.Data.Message                            as Message
import           Flowbox.Bus.Data.Serialize.Proto.Conversion.Message ()
import           Flowbox.Bus.RPC.RPC                                 (RPC)
import           Flowbox.Data.Convert
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                        as Proto
import           Flowbox.UR.Manager.Context                          (Context, ProjectContext(..), Stack, Trans)
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

-- type ProjectID = Int

logger :: LoggerIO
logger = getLoggerIO  $(moduleName)

register :: CorrelationID -> Register.Request -> RPC Context IO Register.Status
register cid request@(Register.Request undoAction redoAction tprojectID tdescription) = do
-- TODO: przerobiæ to tak:
--    let projectID = decodeP tprojectID
--    let description = decodeP tdescription
    reg cid projectID [decodeP undoAction] (decodeP redoAction) description
    return $ Register.Status request True
    where projectID   = decodeP tprojectID
          description = decodeP tdescription

registerMultiple :: CorrelationID -> RegisterMultiple.Request -> RPC Context IO RegisterMultiple.Status
registerMultiple cid request@(RegisterMultiple.Request undoActions redoAction tprojectID tdescription) = do
    let projectID = decodeP tprojectID
    let description = decodeP tdescription
    reg cid projectID (decodeP undoActions) (decodeP redoAction) description
    return $ RegisterMultiple.Status request True

reg :: CorrelationID -> Int -> [Message] -> Message -> String -> RPC Context IO () 
reg cid projectID undoA redoA description = do
    contextMap <- lift get
    pContext <- return $ fromMaybe Context.emptyProjectContext $ Map.lookup projectID contextMap
    let action = (undoA, redoA)
-- TODO: pododawaæ &
        newPC  = (pContext & Context.undo %~ (([action], description, Just $ cid ^. Message.messageID) :))
    logger warning ("action added: " <> description)
    lift $ put $ Map.insert projectID newPC contextMap


undo :: Undo.Request -> RPC Context IO (Undo.Status, Maybe [Message])
undo request@(Undo.Request tprojectID) = execAction (decodeP tprojectID) fst            id   $ Undo.Status request

redo :: Redo.Request -> RPC Context IO (Redo.Status, Maybe [Message])
redo request@(Redo.Request tprojectID) = execAction (decodeP tprojectID) (return . snd) flip $ Redo.Status request 

-- refaktoryzacja? [jak nie wiadomo po 10 min, zawolac WD]
execAction :: Proto.Serializable ret =>
                     -- could be ((a, b) -> c) but "c ~ Message could not be deduced"
              Int -> (Context.Actions -> [Message]) -> 
              ((Stack -> Stack -> (Trans -> ProjectContext)) -> Stack -> Stack -> (Trans -> ProjectContext)) ->
              (Bool-> ret) -> RPC Context IO (ret, Maybe [Message])
execAction projectID accessor invert retCons = do
    contextMap <- lift get
    let projContexts = Map.lookup projectID contextMap 
        ProjectContext stack1 stack2 trans = maybe (Context.emptyProjectContext)
                                                      (\(ProjectContext a b t) -> invert ProjectContext a b $ t)
                                                      projContexts
    case stack1 of
        [] -> 
            return (retCons False, Nothing)
        ((actions, desc, cid) : rest) -> do 
            let newMap = invert ProjectContext rest ((reverse actions, desc, cid) : stack2) $ trans
            lift $ put $ Map.insert projectID newMap contextMap
            logger warning $ "Undo/Redo on " <> desc
            return (retCons True, Just $ actions >>= accessor)


clearStack :: ClearStack.Request -> RPC Context IO ClearStack.Status
clearStack request@(ClearStack.Request tprojectID) = do
    let projectID = decodeP tprojectID
    lift . put . Map.insert projectID Context.emptyProjectContext =<< lift get
    logger warning "Clearing stack"
    return $ ClearStack.Status request 


tBegin :: CorrelationID -> TBegin.Request -> RPC Context IO TBegin.Status
tBegin cid request@(TBegin.Request tprojectID tdescription) = do
    let projectID   = decodeP tprojectID
        description = decodeP tdescription
        pushMid     = maybe Context.emptyProjectContext (Context.trans %~ ((cid ^. Message.messageID, description) :))
    lift . put . Map.alter (Just . pushMid) projectID =<< lift get
    logger warning $ "Opening transaction: " <> description <> " [" <> show cid <> "]"
    return $ TBegin.Status request


tCommit :: TCommit.Request -> RPC Context IO TCommit.Status
tCommit request@(TCommit.Request tprojectID tmessageIDs) = do
    contextMap <- lift get
    let projectID   = decodeP tprojectID

        pc@(ProjectContext undo redo trans) = fromMaybe Context.emptyProjectContext $ Map.lookup projectID contextMap
        newPC       = case trans of
            []     -> pc
            [tmid] -> let cond x      = maybe False (x ==) . (^. _3)
                          (new, rest) = break (cond $ fst tmid) undo
                          (t, notT)   = span (maybe False (flip Set.member $ Set.fromList $ decodeP tmessageIDs) . (^. _3)) new
                          transaction = (concatMap (^. _1) t, snd tmid, Nothing)
                      in  ProjectContext (transaction : notT <> rest) redo []
            (_:cx) -> Context.trans .~ cx $ pc
    logger warning $ maybe "Nothing to commit" (((("closing transaction[" <> (show trans)) <> "]: ") <>) . snd) $ listToMaybe trans
    lift $ put $ Map.insert projectID newPC contextMap
    return $ TCommit.Status request


undoDescriptions :: UDescriptions.Request -> RPC Context IO UDescriptions.Status
undoDescriptions request@(UDescriptions.Request tprojectID) = do
    let projectID = decodeP tprojectID
    oko <- lift get
    let context = Map.lookup projectID oko
    return $ (UDescriptions.Status request) $ maybe (encodeP [""]) (encodeP . map (^. _2) . (^. Context.undo)) context
                                                                                                                                
redoDescriptions :: RDescriptions.Request -> RPC Context IO RDescriptions.Status                                                
redoDescriptions request@(RDescriptions.Request tprojectID) = do
    let projectID = decodeP tprojectID
    oko <- lift get
    let context = Map.lookup projectID oko
    return $ (RDescriptions.Status request) $ maybe (encodeP [""]) (encodeP . map (^. _2) . (^. Context.redo)) context


-- _1, _2 etc zamienic na datatype po nazwach
