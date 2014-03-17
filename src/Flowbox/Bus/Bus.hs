---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes         #-}

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
import           Flowbox.Bus.Topic                  (Topic)
import qualified Flowbox.Bus.Topic                  as Topic
import           Flowbox.Prelude
import qualified Flowbox.Text.ProtocolBuffers       as Proto
import qualified Flowbox.ZMQ.RPC.Client             as Client
import qualified Generated.Proto.Bus.ID.New.Args    as ID_New
import qualified Generated.Proto.Bus.ID.New.Result  as ID_New
import           Generated.Proto.Bus.Request        (Request (Request))
import qualified Generated.Proto.Bus.Request.Method as Method
import qualified Flowbox.Bus.EndPoint as EP

type Error = String

type Bus a = forall z. StateT (BusEnv z) (EitherT Error (ZMQ z)) a


requestClientID :: EP.EndPoint -> EitherT Error (ZMQ z) Message.ClientID
requestClientID addr = do
    socket <- lift $ ZMQ.socket ZMQ.Req
    lift $ ZMQ.connect socket addr
    let request = Extensions.putExt ID_New.req (Just ID_New.Args)
                $ Request Method.ID_New Proto.mkExtField
    response <- Client.query socket request ID_New.rsp
    lift $ ZMQ.close socket
    return $ ID_New.id response


runBus :: MonadIO m => EP.BusEndPoints -> Bus a -> m (Either Error a)
runBus endPoints fun = ZMQ.runZMQ $ runEitherT $ do
    clientID   <- requestClientID $ EP.controlEndPoint endPoints
    subSocket  <- lift $ ZMQ.socket ZMQ.Sub
    pushSocket <- lift $ ZMQ.socket ZMQ.Push
    lift $ ZMQ.connect subSocket  $ EP.pubEndPoint  endPoints
    lift $ ZMQ.connect pushSocket $ EP.pullEndPoint endPoints
    fst <$> (runStateT fun $ BusEnv subSocket pushSocket clientID 0)


getClientID :: Bus Message.ClientID
getClientID = Env.clientID <$> get


getPushSocket :: (Functor f, MonadState (BusEnv z) f) => f (ZMQ.Socket z ZMQ.Push)
getPushSocket = Env.pushSocket <$> get


getSubSocket :: (Functor f, MonadState (BusEnv z) f) => f (ZMQ.Socket z ZMQ.Sub)
getSubSocket  = Env.subSocket <$> get


getNewRequestID :: Bus Message.RequestID
getNewRequestID = do
    s <- get
    let requestID = Env.requestID s
    put $ s { Env.requestID = requestID + 1 }
    return requestID


reply :: Message.CorrelationID -> Message -> Bus ()
reply crlID msg = sendByteString . MessageFrame.toByteString . MessageFrame msg crlID =<< getClientID


send :: Message -> Bus Message.CorrelationID
send msg = do
    requestID <- getNewRequestID
    clientID  <- getClientID
    let correlationID = Message.CorrelationID clientID requestID
    reply correlationID msg
    return correlationID


receive :: Bus (Either String MessageFrame)
receive = MessageFrame.fromByteString <$> receiveByteString


sendByteString :: ByteString -> Bus ()
sendByteString msg = do
    push <- getPushSocket
    lift2 $ ZMQ.send push [] msg


receiveByteString :: Bus ByteString
receiveByteString = do
    sub <- getSubSocket
    lift2 $ ZMQ.receive sub


subscribe :: Topic -> Bus ()
subscribe topic = do
    sub <- getSubSocket
    lift2 $ ZMQ.subscribe sub $ Topic.toByteString topic


unsubscribe :: ByteString -> Bus ()
unsubscribe topic = do
    sub <- getSubSocket
    lift2 $ ZMQ.unsubscribe sub topic

