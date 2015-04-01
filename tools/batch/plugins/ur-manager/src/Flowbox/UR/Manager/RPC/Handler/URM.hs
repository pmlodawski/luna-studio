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
import           Data.Map                                            as Map
import           Data.Maybe                                          (fromMaybe)

import           Flowbox.Bus.Data.Message                            (Message)
import           Flowbox.Bus.Data.Serialize.Proto.Conversion.Message ()
import           Flowbox.Bus.RPC.RPC                                 (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                     hiding (Context, error)
import qualified Flowbox.Text.ProtocolBuffers                        as Proto
import           Flowbox.System.Log.Logger
import           Flowbox.UR.Manager.Context                          (Context, ProjectContext(..), Stack)
import qualified Flowbox.UR.Manager.Context                          as Context
import qualified Generated.Proto.Urm.URM.ClearStack.Request          as ClearStack
import qualified Generated.Proto.Urm.URM.ClearStack.Status           as ClearStack
import qualified Generated.Proto.Urm.URM.Redo.Request                as Redo
import qualified Generated.Proto.Urm.URM.Redo.Status                 as Redo
import qualified Generated.Proto.Urm.URM.Register.Request            as Register
import qualified Generated.Proto.Urm.URM.Register.Status             as Register
import qualified Generated.Proto.Urm.URM.RegisterMultiple.Request    as RegisterMultiple
import qualified Generated.Proto.Urm.URM.RegisterMultiple.Status     as RegisterMultiple
import qualified Generated.Proto.Urm.URM.Undo.Request                as Undo
import qualified Generated.Proto.Urm.URM.Undo.Status                 as Undo


logger :: LoggerIO
logger = getLoggerIO $(moduleName)


register :: Register.Request -> RPC Context IO Register.Status
register request@(Register.Request undoAction redoAction projectID tdescription) = do
    let description = decodeP tdescription
    reg (decodeP projectID) [decodeP undoAction] (decodeP redoAction) description
    return $ Register.Status request True


registerMultiple :: RegisterMultiple.Request -> RPC Context IO RegisterMultiple.Status
registerMultiple request@(RegisterMultiple.Request undoActions redoAction projectID tdescription) = do
    let description = decodeP tdescription
    reg (decodeP projectID) (decodeP undoActions) (decodeP redoAction) description
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
    lift $ put $ Map.insert projectID newContext contextMap


undo :: Undo.Request -> RPC Context IO (Undo.Status, Maybe [Message])
undo request@(Undo.Request projectID) = execAction (decodeP projectID) fst            id   $ Undo.Status request

redo :: Redo.Request -> RPC Context IO (Redo.Status, Maybe [Message])
redo request@(Redo.Request projectID) = execAction (decodeP projectID) (return . snd) flip $ Redo.Status request 

execAction :: Proto.Serializable ret =>
                     -- could be ((a, b) -> c) but "c ~ Message could not be deduced"
              Int -> (Context.Actions -> [Message]) -> ((Stack -> Stack -> (Maybe Context.Transaction -> ProjectContext)) -> Stack -> Stack -> (Maybe Context.Transaction -> ProjectContext)) ->
              (Bool-> ret) -> RPC Context IO (ret, Maybe [Message])
execAction projectID accessor invert retCons = do
    contextMap <- lift get
    let projContexts                   = Map.lookup projectID contextMap 
        ProjectContext stack1 stack2 trans = maybe (Context.emptyProjectContext)
                                                   (\(ProjectContext a b t) -> invert ProjectContext a b $ t)
                                                   projContexts
    case stack1 of
        []              -> return (retCons False, Nothing)
        (action : rest) -> do lift $ put $ Map.insert projectID (invert ProjectContext rest (action : stack2) $ trans) contextMap
                              return (retCons True, Just $ (DList.toList $ fst action) >>= accessor)

clearStack :: ClearStack.Request -> RPC Context IO ClearStack.Status
clearStack request@(ClearStack.Request projectID) = do
    lift . put . Map.insert (decodeP projectID) Context.emptyProjectContext =<< lift get
    return $ ClearStack.Status request 
