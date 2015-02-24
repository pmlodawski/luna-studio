---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Flowbox.UR.Manager.RPC.Handler.URM where

import qualified Data.ByteString.Char8 as Char8
import           Data.Maybe                      (listToMaybe)
import           Control.Monad.Trans.State.Lazy

import           Flowbox.Bus.Data.Message                            (Message)
import qualified Flowbox.Bus.Data.Message                            as Message
import           Flowbox.Bus.Data.Serialize.Proto.Conversion.Message ()
import           Flowbox.Bus.RPC.RPC                                 (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                     hiding (Context, error)
import           Flowbox.System.Log.Logger
import           Flowbox.UR.Manager.Context                          as Context
import           Flowbox.UR.Manager.Context                          (Context)
import qualified Generated.Proto.Urm.URM.Redo.Request                as Redo
import qualified Generated.Proto.Urm.URM.Redo.Status                 as Redo
import qualified Generated.Proto.Urm.URM.Register.Request            as Register
import qualified Generated.Proto.Urm.URM.Register.Status             as Register
import qualified Generated.Proto.Urm.URM.Undo.Request                as Undo
import qualified Generated.Proto.Urm.URM.Undo.Status                 as Undo


logger :: LoggerIO
logger = getLoggerIO $(moduleName)


register :: Register.Request -> RPC Context IO Register.Status
register request@(Register.Request undoAction) = do
    Context undo redo <- lift get
    logger info $ show redo
    case listToMaybe redo of
        Just undone -> 
            return $ Register.Status request False
        _           -> do
            let h = Context (decodeP undoAction : undo) []
            lift $ put h
            return $ Register.Status request True


undo :: Undo.Request -> RPC Context IO (Undo.Status, Maybe Message)
undo request = do
    Context undo redo <- lift get
    logger info $ show undo
    case undo of
        []            -> do
            return $ (Undo.Status request False, Nothing)
        action : rest -> do
            lift $ put $ Context rest $ action : redo
            return $ (Undo.Status request True, Just action)

redo :: Redo.Request -> RPC Context IO (Redo.Status, Maybe Message)
redo request = do
    Context undo redo <- lift get
    logger info $ show undo
    case redo of
        []               -> do
            return (Redo.Status request False, Nothing)
        action : rest -> do
            lift $ put $ Context undo rest
            return $ (Redo.Status request True, Just action)
