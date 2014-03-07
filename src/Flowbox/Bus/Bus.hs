---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}

module Flowbox.Bus.Bus where

import           Control.Monad.Reader
import           System.ZMQ4.Monadic             (ZMQ)
import qualified System.ZMQ4.Monadic             as ZMQ
import qualified Text.ProtocolBuffers.Extensions as Extensions

import           Control.Monad.Trans.Either
import           Flowbox.Bus.Env                    (BusEnv (BusEnv))
import qualified Flowbox.Bus.Env                    as Env
import           Flowbox.Prelude
import qualified Flowbox.Text.ProtocolBuffers       as Proto
import qualified Flowbox.ZMQ.RPC.Client             as Client
import qualified Generated.Proto.Bus.ID.New.Args    as ID_New
import qualified Generated.Proto.Bus.ID.New.Result  as ID_New
import           Generated.Proto.Bus.Request        (Request (Request))
import qualified Generated.Proto.Bus.Request.Method as Method



type Error = String

type Bus a = forall z. ReaderT BusEnv (EitherT Error (ZMQ z)) a



getClientID :: Env.EndPoint -> EitherT Error (ZMQ z) Env.ClientID
getClientID addr = do
    socket <- lift $ ZMQ.socket ZMQ.Req
    lift $ ZMQ.connect socket addr
    let request = Extensions.putExt ID_New.req (Just ID_New.Args)
                $ Request Method.ID_New Proto.mkExtField
    response <- Client.query socket request ID_New.rsp
    lift $ ZMQ.close socket
    return $ ID_New.id response


runBus :: ReaderT BusEnv (EitherT Error (ZMQ z)) a
       -> Env.BusEndPoints
       -> ZMQ z (Either Error a)
runBus fun endPoints = runEitherT $ runBus' fun endPoints


runBus' :: ReaderT BusEnv (EitherT Error (ZMQ z)) a
        -> Env.BusEndPoints -> EitherT Error (ZMQ z) a
runBus' fun endPoints = do
    clientID <- getClientID $ Env.controlEndPoint endPoints
    runReaderT fun $ BusEnv endPoints clientID


test :: Bus Int
test = do
    socket <- (lift . lift) $ ZMQ.socket ZMQ.Req
    print "test"
    x <- ask
    print x
    return 5


main :: IO ()
main = do
    x <- ZMQ.runZMQ $ runBus test (Env.BusEndPoints "tcp://127.0.0.1:30530" "" "")
    print x
    return ()

