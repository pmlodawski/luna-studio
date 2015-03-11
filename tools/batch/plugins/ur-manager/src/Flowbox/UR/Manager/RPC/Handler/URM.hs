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
register request@(Register.Request undoAction redoAction projectID) = do
    reg (decodeP projectID) [decodeP undoAction] $ decodeP redoAction
    return $ Register.Status request True


registerMultiple :: RegisterMultiple.Request -> RPC Context IO RegisterMultiple.Status
registerMultiple request@(RegisterMultiple.Request undoActions redoAction projectID) = do
    reg (decodeP projectID) (decodeP undoActions) $ decodeP redoAction
    return $ RegisterMultiple.Status request True

reg :: Int -> [Message] -> Message -> RPC Context IO ()
reg projectID undoA redoA = do
    contextMap <- lift get
    ProjectContext undoL _ <- return $ fromMaybe (ProjectContext [] []) $ Map.lookup projectID contextMap
    lift $ put $ Map.insert projectID (ProjectContext ((undoA, redoA) : undoL) []) contextMap


undo :: Undo.Request -> RPC Context IO (Undo.Status, Maybe [Message])
undo request@(Undo.Request projectID) = execAction (decodeP projectID) fst            id   $ Undo.Status request

redo :: Redo.Request -> RPC Context IO (Redo.Status, Maybe [Message])
redo request@(Redo.Request projectID) = execAction (decodeP projectID) (return . snd) flip $ Redo.Status request 

execAction :: Proto.Serializable ret =>
                     -- could be ((a, b) -> c) but "c ~ Message could not be deduced"
              Int -> (([Message], Message) -> [Message]) -> ((Stack -> Stack -> ProjectContext) -> Stack -> Stack -> ProjectContext) ->
              (Bool-> ret) -> RPC Context IO (ret, Maybe [Message])
execAction projectID accessor invert retCons = do
    contextMap <- lift get
    let projContexts                 = Map.lookup projectID contextMap 
        ProjectContext stack1 stack2 = maybe (ProjectContext [] [])
                                             (\(ProjectContext a b) -> invert ProjectContext a b)
                                             projContexts
    case stack1 of
        []              -> return (retCons False, Nothing)
        (action : rest) -> do lift $ put $ Map.insert projectID (invert ProjectContext rest $ action : stack2) contextMap
                              return (retCons True, Just $ accessor action)
