{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.UR.Manager.RPC.Handler.URM where

import           Control.Monad.Trans.State (get, put)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe, listToMaybe, maybeToList)
import qualified Data.Set                  as Set

import Flowbox.Prelude hiding (Context, error)

import           Flowbox.Bus.Data.Message                                 (CorrelationID, Message)
import qualified Flowbox.Bus.Data.Message                                 as Message
import           Flowbox.Bus.Data.Serialize.Proto.Conversion.Message      ()
import           Flowbox.Bus.RPC.RPC                                      (RPC)
import           Flowbox.Data.Convert
import           Flowbox.System.Log.Logger
import           Flowbox.UR.Manager.Context                               (Context, ProjectContext (..), Stack)
import qualified Flowbox.UR.Manager.Context                               as Context
import           Flowbox.UR.Manager.RPC.Topic                             as Topic
import qualified Generated.Proto.Urm.URM.ClearStack.Request               as ClearStack
import qualified Generated.Proto.Urm.URM.ClearStack.Status                as ClearStack
import qualified Generated.Proto.Urm.URM.Descriptions.Cleared.Update      as UDescCleared
import qualified Generated.Proto.Urm.URM.Redo.Descriptions.Removed.Update as RDescRemoved
import qualified Generated.Proto.Urm.URM.Redo.Descriptions.Request        as RDescriptions
import qualified Generated.Proto.Urm.URM.Redo.Descriptions.Status         as RDescriptions
import qualified Generated.Proto.Urm.URM.Redo.Request                     as Redo
import qualified Generated.Proto.Urm.URM.Redo.Status                      as Redo
import qualified Generated.Proto.Urm.URM.Register.Request                 as Register
import qualified Generated.Proto.Urm.URM.Register.Status                  as Register
import qualified Generated.Proto.Urm.URM.RegisterMultiple.Request         as RegisterMultiple
import qualified Generated.Proto.Urm.URM.RegisterMultiple.Status          as RegisterMultiple
import qualified Generated.Proto.Urm.URM.Transaction.Begin.Request        as TBegin
import qualified Generated.Proto.Urm.URM.Transaction.Begin.Status         as TBegin
import qualified Generated.Proto.Urm.URM.Transaction.Commit.Request       as TCommit
import qualified Generated.Proto.Urm.URM.Transaction.Commit.Status        as TCommit
import qualified Generated.Proto.Urm.URM.Undo.Descriptions.Added.Update   as UDescAdded
import qualified Generated.Proto.Urm.URM.Undo.Descriptions.Removed.Update as UDescRemoved
import qualified Generated.Proto.Urm.URM.Undo.Descriptions.Request        as UDescriptions
import qualified Generated.Proto.Urm.URM.Undo.Descriptions.Status         as UDescriptions
import qualified Generated.Proto.Urm.URM.Undo.Request                     as Undo
import qualified Generated.Proto.Urm.URM.Undo.Status                      as Undo

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

type ProjectID = Int

logger :: LoggerIO
logger = getLoggerIO  $moduleName

--------------------------------------------------------------------------------
-- Messages registration
--------------------------------------------------------------------------------

register :: CorrelationID -> Register.Request -> RPC Context IO (Register.Status, [Message])
register cid request@(Register.Request undoAction redoAction tprojectID tdescription) = do
    descNotific <- reg cid projectID [decodeP undoAction] (decodeP redoAction) description
    return (Register.Status request True, maybeToList descNotific)
    where projectID   = decodeP tprojectID
          description = decodeP tdescription

registerMultiple :: CorrelationID -> RegisterMultiple.Request -> RPC Context IO (RegisterMultiple.Status, [Message])
registerMultiple cid request@(RegisterMultiple.Request undoActions redoAction tprojectID tdescription) = do
    descNotific <- reg cid projectID (decodeP undoActions) (decodeP redoAction) description
    return (RegisterMultiple.Status request True, maybeToList descNotific)
    where projectID   = decodeP tprojectID
          description = decodeP tdescription

reg :: CorrelationID -> ProjectID -> [Message] -> Message -> String -> RPC Context IO (Maybe Message)
reg cid projectID undoA redoA description = do
    contextMap <- lift get
    logger debug ("action added: " <> description)
    let ProjectContext undoL _ trans = fromMaybe def $ Map.lookup projectID contextMap
        newUndo     = Context.Package [action] metadata : undoL
        newProjCtxt = ProjectContext newUndo [] trans
    lift $ put $ Map.insert projectID newProjCtxt contextMap
    return (if null trans then Just descNotific
                          else Nothing)
    where action      = Context.Actions undoA redoA
          metadata    = Context.Metadata description $ Just $ cid ^. Message.messageID
          descNotific = Message.mk Topic.urmUndoDescriptionAdded $ UDescAdded.Update (encodeP projectID) (encodeP description)

--------------------------------------------------------------------------------
-- Undo / redo execution
--------------------------------------------------------------------------------

undo :: Undo.Request -> RPC Context IO (Undo.Status, [Message])
undo request@(Undo.Request tprojectID) = do
    actions <- execAction projectID
                          Context.undoStack
                          Context.redoStack
    let messages = concatMap (^. Context.undo) actions
    case actions of
        [] -> return (Undo.Status request False, [])
        _  -> return (Undo.Status request True , descNotific : messages)
    where projectID   = decodeP tprojectID
          descNotific = Message.mk Topic.urmUndoDescriptionRemoved $ UDescRemoved.Update tprojectID

redo :: Redo.Request -> RPC Context IO (Redo.Status, [Message])
redo request@(Redo.Request tprojectID) = do
    actions <- execAction projectID
                          Context.redoStack
                          Context.undoStack
    let messages = map (^. Context.redo) actions
    case actions of
        [] -> return (Redo.Status request False, [])
        _  -> return (Redo.Status request True , descNotific : messages)
    where projectID   = decodeP tprojectID
          descNotific = Message.mk Topic.urmRedoDescriptionRemoved $ RDescRemoved.Update tprojectID

execAction :: ProjectID -> Lens' ProjectContext Stack -> Lens' ProjectContext Stack -> RPC Context IO [Context.Actions]
execAction projectID source dest = do
    contextMap <- lift get
    let projContext = fromMaybe def $ Map.lookup projectID contextMap
    case projContext ^. Context.trans of
        [] -> case projContext ^. source of
                  [] -> return []
                  (pkg : cs) -> do
                      logger debug $ "Undo/Redo on " <> description
                      lift $ put $ Map.insert projectID newProjContext contextMap
                      return actions
                      where newDest        = (pkg & Context.actions %~ reverse) : (projContext ^. dest) :: Stack
                            setDest        = projContext & dest   .~ newDest                            :: ProjectContext
                            newProjContext = setDest     & source .~ cs                                 :: ProjectContext
                            actions        = pkg ^. Context.actions                                     :: [Context.Actions]
                            description    = pkg ^. Context.metadata ^. Context.description
        _  -> return []

--------------------------------------------------------------------------------
-- Extra stack managment
--------------------------------------------------------------------------------

clearStack :: ClearStack.Request -> RPC Context IO (ClearStack.Status, [Message])
clearStack request@(ClearStack.Request tprojectID) = do
    lift . put . Map.insert projectID def =<< lift get
    logger debug "Clearing stack"
    return (ClearStack.Status request, [descNotific])
    where projectID = decodeP tprojectID
          descNotific = Message.mk Topic.urmDescriptionsCleared $ UDescCleared.Update tprojectID

--------------------------------------------------------------------------------
-- Transactions
--------------------------------------------------------------------------------

tBegin :: CorrelationID -> TBegin.Request -> RPC Context IO TBegin.Status
tBegin cid request@(TBegin.Request tprojectID tdescription) = do
    lift . put . Map.alter (Just . pushMid) projectID =<< lift get
    logger debug $ "Opening transaction: " <> description <> " [" <> show cid <> "]"
    return $ TBegin.Status request
    where projectID   = decodeP tprojectID
          description = decodeP tdescription
          transInfo   = Context.Metadata description (Just $ cid ^. Message.messageID)
          pushMid     = maybe def (Context.trans %~ (transInfo :))

tCommit :: TCommit.Request -> RPC Context IO (TCommit.Status, [Message])
tCommit request@(TCommit.Request tprojectID tmessageIDs) = do
    contextMap <- lift get
    let projContext@(ProjectContext undoL _ trans) = fromMaybe def $ Map.lookup projectID contextMap
        (newProjContext, descNotific) = case trans of
            []     -> (projContext, [])
            [meta] -> (newPContext, [notification])
                      where Just beginCID             = meta ^. Context.cid
                            (preBegin, postBegin)     = break (maybe False (beginCID ==) . pkgCid) undoL
                            transIDs                  = Set.fromList $ decodeP tmessageIDs
                            (transElem, notTransElem) = span (maybe False (`Set.member` transIDs) . pkgCid) preBegin
                            transInfo                 = Context.Metadata description Nothing
                            transPackage              = Context.Package (concatMap (^. Context.actions) transElem) transInfo
                            description               = meta ^. Context.description
                            notification              = descNotificMk $ encodeP description
                            newPContext               = ProjectContext (transPackage : notTransElem <> postBegin) [] []
            (_:cx) -> (Context.trans .~ cx $ projContext, [])
    logger debug $ maybe "Nothing to commit" ((("closing transaction[" <> show trans <> "]: ") <>) . (^. Context.description)) $ listToMaybe trans
    lift $ put $ Map.insert projectID newProjContext contextMap
    return (TCommit.Status request, descNotific)
    where pkgCid    = (^. Context.cid) . (^. Context.metadata)
          projectID = decodeP tprojectID
          descNotificMk = Message.mk Topic.urmUndoDescriptionAdded . UDescAdded.Update tprojectID

--------------------------------------------------------------------------------
-- Desctiptions fetching
--------------------------------------------------------------------------------

undoDescriptions :: UDescriptions.Request -> RPC Context IO UDescriptions.Status
undoDescriptions request@(UDescriptions.Request tprojectID) = do
    let projectID = decodeP tprojectID
    oko <- lift get
    let context = Map.lookup projectID oko
    return $ UDescriptions.Status request $ (encodeP . maybe [""] (map descFromPkg . (^. Context.undoStack))) context
    where descFromPkg = (^. Context.description) . (^. Context.metadata)

redoDescriptions :: RDescriptions.Request -> RPC Context IO RDescriptions.Status
redoDescriptions request@(RDescriptions.Request tprojectID) = do
    let projectID = decodeP tprojectID
    contextMap <- lift get
    let context = Map.lookup projectID contextMap
    return $ RDescriptions.Status request $ (encodeP . maybe [""] (map descFromPkg . (^. Context.redoStack))) context
    where
        descFromPkg = (^. Context.description) . (^. Context.metadata)


-- _1, _2 etc zamienic na datatype po nazwach
