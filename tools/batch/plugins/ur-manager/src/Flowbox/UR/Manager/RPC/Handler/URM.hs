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
import qualified Generated.Proto.Urm.URM.Register.Request            as Register
import qualified Generated.Proto.Urm.URM.Register.Status             as Register
import qualified Generated.Proto.Urm.URM.Undo.Request                as Undo
import qualified Generated.Proto.Urm.URM.Undo.Status                 as Undo


logger :: LoggerIO
logger = getLoggerIO $(moduleName)


register :: Register.Request -> RPC Context IO Register.Status
register request@(Register.Request undoAction) = do
    Context undo redo <- lift get
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
    case undo of
        []            -> do
            return $ (Undo.Status request False, Nothing)
        action : rest -> do
            lift $ put $ Context rest $ action : redo
            return $ (Undo.Status request True, Just action)

