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

import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as Char8
import           System.ZMQ4.Monadic             (ZMQ)
import qualified System.ZMQ4.Monadic             as ZMQ
import qualified Text.ProtocolBuffers.Extensions as Extensions

import           Flowbox.Bus.Env                    (BusEnv (BusEnv))
import qualified Flowbox.Bus.Env                    as Env
import qualified Flowbox.Bus.Message                as Message
import           Flowbox.Bus.MessageFrame           (MessageFrame)
import qualified Flowbox.Bus.MessageFrame           as MessageFrame
import           Flowbox.Prelude
import qualified Flowbox.Text.ProtocolBuffers       as Proto
import qualified Flowbox.ZMQ.RPC.Client             as Client
import qualified Generated.Proto.Bus.ID.New.Args    as ID_New
import qualified Generated.Proto.Bus.ID.New.Result  as ID_New
import           Generated.Proto.Bus.Request        (Request (Request))
import qualified Generated.Proto.Bus.Request.Method as Method



type Error = String

type Bus a = forall z. ReaderT (BusEnv z) (EitherT Error (ZMQ z)) a


requestClientID :: Env.EndPoint -> EitherT Error (ZMQ z) Message.ClientID
requestClientID addr = do
    socket <- lift $ ZMQ.socket ZMQ.Req
    lift $ ZMQ.connect socket addr
    let request = Extensions.putExt ID_New.req (Just ID_New.Args)
                $ Request Method.ID_New Proto.mkExtField
    response <- Client.query socket request ID_New.rsp
    lift $ ZMQ.close socket
    return $ ID_New.id response


runBus :: ReaderT (BusEnv z) (EitherT Error (ZMQ z)) a
       -> Env.BusEndPoints
       -> ZMQ z (Either Error a)
runBus fun endPoints = runEitherT $ runBus' fun endPoints


runBus' :: ReaderT (BusEnv z) (EitherT Error (ZMQ z)) a
        -> Env.BusEndPoints -> EitherT Error (ZMQ z) a
runBus' fun endPoints = do
    clientID <- requestClientID $ Env.controlEndPoint endPoints
    subSocket  <- lift $ ZMQ.socket ZMQ.Sub
    pushSocket <- lift $ ZMQ.socket ZMQ.Push
    lift $ ZMQ.connect subSocket  $ Env.pubEndPoint  endPoints
    lift $ ZMQ.connect pushSocket $ Env.pullEndPoint endPoints
    runReaderT fun $ BusEnv subSocket pushSocket clientID


getClientID :: Bus Message.ClientID
getClientID = Env.clientID <$> ask


getPushSocket :: (Functor f, MonadReader (BusEnv z) f) => f (ZMQ.Socket z ZMQ.Push)
getPushSocket = Env.pushSocket <$> ask


getSubSocket :: (Functor f, MonadReader (BusEnv z) f) => f (ZMQ.Socket z ZMQ.Sub)
getSubSocket  = Env.subSocket <$> ask


send :: MessageFrame -> Bus ()
send = send' . MessageFrame.toByteString


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


test :: Bus ()
test = do
    subscribe $ Char8.pack ""
    putStrLn "sending.."
    send' $ Char8.pack "test test"
    _ <- forever $ do
        putStrLn "receiving.."
        x <- receive'
        return ()
    putStrLn "finishing"
    return ()


main :: IO ()
main = do
    x <- ZMQ.runZMQ $ runBus test (Env.BusEndPoints "tcp://127.0.0.1:30530"
                                                    "tcp://127.0.0.1:30531"
                                                    "tcp://127.0.0.1:30532"
                                  )
    print x
    return ()

