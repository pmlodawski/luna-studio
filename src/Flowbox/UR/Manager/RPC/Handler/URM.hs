---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Flowbox.UR.Manager.RPC.Handler.URM where

import           Control.Monad.Trans.State.Lazy

import           Flowbox.Bus.Data.Message                            (Message)
import           Flowbox.Bus.Data.Serialize.Proto.Conversion.Message ()
import           Flowbox.Bus.RPC.RPC                                 (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                     hiding (Context, error)
import           Flowbox.System.Log.Logger
import           Flowbox.UR.Manager.Context                          as Context
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
register request@(Register.Request undoAction redoAction) = do
    reg [decodeP undoAction] $ decodeP redoAction
    return $ Register.Status request True


registerMultiple :: RegisterMultiple.Request -> RPC Context IO RegisterMultiple.Status
registerMultiple request@(RegisterMultiple.Request undoActions redoAction) = do
    reg (decodeP undoActions) $ decodeP redoAction
    return $ RegisterMultiple.Status request True

reg :: [Message] -> Message -> RPC Context IO ()
reg undoA redoA = do
    Context undoL redoL <- lift get
    logger info $ "register: undo " ++ (show $ length undoL) ++ " redo " ++ (show $ length redoL)
    lift $ put $ Context ((undoA, redoA) : undoL) []


undo :: Undo.Request -> RPC Context IO (Undo.Status, Maybe [Message])
undo request = do
    Context undoL redoL <- lift get
    logger info $ "undo: undo " ++ (show $ length undoL) ++ " redo " ++ (show $ length redoL)
    case undoL of
        []            -> do
            return $ (Undo.Status request False, Nothing)
        action : rest -> do
            lift $ put $ Context rest $ action : redoL
            return $ (Undo.Status request True, Just $ fst action)

redo :: Redo.Request -> RPC Context IO (Redo.Status, Maybe [Message])
redo request = do
    Context undoL redoL <- lift get
    logger info $ "redo: undo " ++ (show $ length undoL) ++ " redo " ++ (show $ length redoL)
    case redoL of
        []               -> do
            return (Redo.Status request False, Nothing)
        action : rest -> do
            lift $ put $ Context (action : undoL) rest
            return $ (Redo.Status request True, Just $ [snd action])
