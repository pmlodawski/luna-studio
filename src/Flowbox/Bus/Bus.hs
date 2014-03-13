---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Flowbox.Bus.Bus where

import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.ByteString                 (ByteString)
import           System.ZMQ4.Monadic             (ZMQ)
import qualified System.ZMQ4.Monadic             as ZMQ
import qualified Text.ProtocolBuffers.Extensions as Extensions

import           Flowbox.Bus.Env                    (BusEnv (BusEnv))
import qualified Flowbox.Bus.Env                    as Env
import           Flowbox.Bus.Message                (Message)
import qualified Flowbox.Bus.Message                as Message
import           Flowbox.Bus.MessageFrame           (MessageFrame (MessageFrame))
import qualified Flowbox.Bus.MessageFrame           as MessageFrame
import           Flowbox.Prelude
import qualified Flowbox.Text.ProtocolBuffers       as Proto
import qualified Flowbox.ZMQ.RPC.Client             as Client
import qualified Generated.Proto.Bus.ID.New.Args    as ID_New
import qualified Generated.Proto.Bus.ID.New.Result  as ID_New
import           Generated.Proto.Bus.Request        (Request (Request))
import qualified Generated.Proto.Bus.Request.Method as Method



type Error = String

type Bus a = forall z. StateT (BusEnv z) (EitherT Error (ZMQ z)) a


requestClientID :: Env.EndPoint -> EitherT Error (ZMQ z) Message.ClientID
requestClientID addr = do
    socket <- lift $ ZMQ.socket ZMQ.Req
    lift $ ZMQ.connect socket addr
    let request = Extensions.putExt ID_New.req (Just ID_New.Args)
                $ Request Method.ID_New Proto.mkExtField
    response <- Client.query socket request ID_New.rsp
    lift $ ZMQ.close socket
    return $ ID_New.id response


runBus :: StateT (BusEnv z) (EitherT Error (ZMQ z)) a
       -> Env.BusEndPoints
       -> ZMQ z (Either Error a)
runBus fun endPoints = runEitherT $ runBus' fun endPoints


runBus' :: StateT (BusEnv z) (EitherT Error (ZMQ z)) a
        -> Env.BusEndPoints -> EitherT Error (ZMQ z) a
runBus' fun endPoints = do
    clientID <- requestClientID $ Env.controlEndPoint endPoints
    subSocket  <- lift $ ZMQ.socket ZMQ.Sub
    pushSocket <- lift $ ZMQ.socket ZMQ.Push
    lift $ ZMQ.connect subSocket  $ Env.pubEndPoint  endPoints
    lift $ ZMQ.connect pushSocket $ Env.pullEndPoint endPoints
    fst <$> (runStateT fun $ BusEnv subSocket pushSocket clientID 0)


getClientID :: Bus Message.ClientID
getClientID = Env.clientID <$> get


getPushSocket :: (Functor f, MonadState (BusEnv z) f) => f (ZMQ.Socket z ZMQ.Push)
getPushSocket = Env.pushSocket <$> get


getSubSocket :: (Functor f, MonadState (BusEnv z) f) => f (ZMQ.Socket z ZMQ.Sub)
getSubSocket  = Env.subSocket <$> get


getNewRequestID :: Bus Message.RequestID
getNewRequestID = do s <- get 
                     let requestID = Env.requestID s
                     put $ s { Env.requestID = requestID + 1 }
                     return requestID


reply :: (Message, Message.CorrelationID) -> Bus ()
reply (msg, crlID) = do
    clientID <- getClientID
    send' $ MessageFrame.toByteString $ MessageFrame msg crlID clientID


send :: Message -> Bus Message.CorrelationID
send msg = do
    requestID <- getNewRequestID
    clientID  <- getClientID
    let correlationID = Message.CorrelationID clientID requestID
    reply (msg, correlationID)
    return correlationID


receive :: Bus (Either String MessageFrame)
receive = MessageFrame.fromByteString <$> receive'


send' :: ByteString -> Bus ()
send' msg = do
    push <- getPushSocket
    lift $ lift $ ZMQ.send push [] msg


receive' :: Bus ByteString
receive' = do
    sub <- getSubSocket
    lift $ lift $ ZMQ.receive sub


subscribe :: ByteString -> Bus ()
subscribe topic = do
    sub <- getSubSocket
    lift $ lift $ ZMQ.subscribe sub topic


unsubscribe :: ByteString -> Bus ()
unsubscribe topic = do
    sub <- getSubSocket
    lift $ lift $ ZMQ.unsubscribe sub topic

