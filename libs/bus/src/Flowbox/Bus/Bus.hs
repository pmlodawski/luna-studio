---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Flowbox.Bus.Bus where

import qualified Control.Concurrent              as Concurrent
import qualified Control.Concurrent.Async        as Async
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.ByteString                 (ByteString)
import           System.ZMQ4.Monadic             (ZMQ)
import qualified System.ZMQ4.Monadic             as ZMQ
import qualified Text.ProtocolBuffers.Extensions as Extensions

import           Flowbox.Bus.Data.Flag                (Flag)
import           Flowbox.Bus.Data.Message             (Message)
import qualified Flowbox.Bus.Data.Message             as Message
import           Flowbox.Bus.Data.MessageFrame        (MessageFrame (MessageFrame))
import qualified Flowbox.Bus.Data.MessageFrame        as MessageFrame
import           Flowbox.Bus.Data.Topic               (Topic)
import qualified Flowbox.Bus.Data.Topic               as Topic
import qualified Flowbox.Bus.EndPoint                 as EP
import           Flowbox.Bus.Env                      (BusEnv (BusEnv))
import qualified Flowbox.Bus.Env                      as Env
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers         as Proto
import qualified Flowbox.ZMQ.RPC.Client               as Client
import qualified Generated.Proto.Bus.ID.Create.Args   as ID_Create
import qualified Generated.Proto.Bus.ID.Create.Result as ID_Create
import           Generated.Proto.Bus.Request          (Request (Request))
import qualified Generated.Proto.Bus.Request.Method   as Method



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.Bus"


type Error = String

type Bus a = forall z. StateT (BusEnv z) (EitherT Error (ZMQ z)) a


requestClientID :: EP.EndPoint -> EitherT Error (ZMQ z) Message.ClientID
requestClientID addr = do
    socket <- lift $ ZMQ.socket ZMQ.Req
    lift $ ZMQ.connect socket addr
    let request = Extensions.putExt ID_Create.req (Just ID_Create.Args)
                $ Request Method.ID_Create Proto.mkExtField
    response <- Client.query socket request ID_Create.rsp
    lift $ ZMQ.close socket
    return $ ID_Create.id response


runBus :: MonadIO m => EP.BusEndPoints -> Bus a -> m (Either Error a)
runBus endPoints fun = ZMQ.runZMQ $ runEitherT $ do
    logger trace "Connecting to bus..."
    clientID   <- requestClientID $ EP.controlEndPoint endPoints
    subSocket  <- lift $ ZMQ.socket ZMQ.Sub
    pushSocket <- lift $ ZMQ.socket ZMQ.Push
    lift $ ZMQ.connect subSocket  $ EP.pubEndPoint  endPoints
    lift $ ZMQ.connect pushSocket $ EP.pullEndPoint endPoints
    logger trace "Connected to bus"
    fst <$> runStateT fun (BusEnv subSocket pushSocket clientID 0)


getClientID :: Bus Message.ClientID
getClientID = Env.clientID <$> get


getPushSocket :: (Functor f, MonadState (BusEnv z) f) => f (ZMQ.Socket z ZMQ.Push)
getPushSocket = Env.pushSocket <$> get


getSubSocket :: (Functor f, MonadState (BusEnv z) f) => f (ZMQ.Socket z ZMQ.Sub)
getSubSocket = Env.subSocket <$> get


getNewRequestID :: Bus Message.RequestID
getNewRequestID = do
    s <- get
    let requestID = Env.requestID s
    put $ s { Env.requestID = requestID + 1 }
    return requestID


reply :: Message.CorrelationID -> Flag -> Message -> Bus ()
reply crlID lastFrame msg = do
    clientID <- getClientID
    sendByteString $ MessageFrame.toByteString $ MessageFrame msg crlID clientID lastFrame


send :: Flag -> Message -> Bus Message.CorrelationID
send lastFrame msg = do
    correlationID <- Message.CorrelationID <$> getClientID <*> getNewRequestID
    reply correlationID lastFrame msg
    return correlationID


receive :: Bus (Either Error MessageFrame)
receive = MessageFrame.fromByteString <$> receiveByteString


receive' :: Bus MessageFrame
receive' = receive >>= lift . hoistEither


withTimeout :: Bus a -> Int -> Bus (Either Error a)
withTimeout action timeout = runEitherT $ do
    state' <- get
    task <- lift3 $ ZMQ.async $ runEitherT $ runStateT action state'
    wait <- liftIO $ Async.async $ do Concurrent.threadDelay timeout
                                      return "Timeout reached"
    r <- liftIO $ Async.waitEitherCancel wait task
    (result, newState) <- hoistEither $ join r
    put newState
    return result


sendByteString :: ByteString -> Bus ()
sendByteString msg = do
    push <- getPushSocket
    logger trace "Sending message..."
    lift2 $ ZMQ.send push [] msg
    logger trace "Message sent"


receiveByteString :: Bus ByteString
receiveByteString = do
    sub <- getSubSocket
    logger trace "Waiting for a message..."
    bs <- lift2 $ ZMQ.receive sub
    logger trace "Message received"
    return bs


subscribe :: Topic -> Bus ()
subscribe topic = do
    sub <- getSubSocket
    lift2 $ ZMQ.subscribe sub $ Topic.toByteString topic


unsubscribe :: Topic -> Bus ()
unsubscribe topic = do
    sub <- getSubSocket
    lift2 $ ZMQ.unsubscribe sub $ Topic.toByteString topic

