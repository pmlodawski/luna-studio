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
import           Control.Monad.Trans.State.Lazy

import           Flowbox.Bus.Data.Message                            (Message)
import qualified Flowbox.Bus.Data.Message                            as Message
import           Flowbox.Bus.Data.Serialize.Proto.Conversion.Message ()
import           Flowbox.Bus.RPC.RPC                                 (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                     hiding (Context, error)
import           Flowbox.System.Log.Logger
import           Flowbox.UR.Manager.Context                          as Context
import qualified Generated.Proto.Urm.URM.Undo.Register.Request       as Register
import qualified Generated.Proto.Urm.URM.Undo.Register.Status        as Register
import qualified Generated.Proto.Urm.URM.Undo.Perform.Request        as Undo
import qualified Generated.Proto.Urm.URM.Undo.Perform.Status         as Undo

logger :: LoggerIO
logger = getLoggerIO $(moduleName)

undo2 :: Undo.Request -> RPC Context IO Undo.Status
undo2 request = do
--    assert False 'a'
--    let str = decodeP tstr
    logger info "Odpaliło!"
    return $ Undo.Status request

register :: Register.Request -> RPC Context IO Register.Status
register request@(Register.Request undoAction) = do
    logger info $ "zaladowalem " ++ (show request)
    asd <- lift get
    let message = decodeP undoAction
    lift $ put $ message : asd
    return $ Register.Status request

undo :: Undo.Request -> RPC Context IO (Undo.Status, Message)
undo request = do
    logger info $ "cofam " ++ (show request)
    asd <- lift get
--    case (asd ^. Env.times) of
    case asd of
        []            -> return $ (Undo.Status request, (Message.Message "urm.undo.perform.request" (Char8.pack "")))
        action : rest -> do
            lift $ put rest
            return $ (Undo.Status request, action)
