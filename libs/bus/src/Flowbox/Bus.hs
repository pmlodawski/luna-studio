

-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Bus where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified System.ZMQ4.Monadic             as ZMQ
import qualified Text.ProtocolBuffers.Extensions as Extensions

import           Control.Monad.Trans.Either
import           Flowbox.Prelude
import qualified Flowbox.Text.ProtocolBuffers       as Proto
import qualified Flowbox.ZMQ.RPC.Client             as Client
import qualified Generated.Proto.Bus.ID.New.Args    as ID_New
import qualified Generated.Proto.Bus.ID.New.Result  as ID_New
import           Generated.Proto.Bus.Request        (Request (Request))
import qualified Generated.Proto.Bus.Request        as Request
import qualified Generated.Proto.Bus.Request.Method as Method

type ClientID = Proto.Int32

type EndPoint = String

data BusEndPoints = BusEndPoints { controlEndPoint :: EndPoint
                                 , pullEndPoint    :: EndPoint
                                 , pubEndPoint     :: EndPoint
                                 } deriving (Read, Show, Eq)

data BusEnv = BusEnv { endPoints :: BusEndPoints
                     , clientID  :: ClientID
                     } deriving (Read, Show, Eq)


type BusMonad m = (MonadIO m, MonadReader BusEnv m)


--getClientID :: MonadIO m => EndPoint -> EitherT String m ClientID
--getClientID addr = ZMQ.runZMQ $ do
--    socket <- ZMQ.socket ZMQ.Req
--    ZMQ.connect socket addr
--    let request = Extensions.putExt ID_New.req (Just undefined)
--                $ Request Method.ID_New Proto.mkExtField
--    response <- Client.query socket request ID_New.rsp
--    fmap ID_New.id response


--runBus :: MonadIO m => ReaderT BusEnv m a -> BusEndPoints -> EitherT String m a
--runBus f ep = do
--    cliID <- getClientID $ controlEndPoint ep
--    out   <- runReaderT f $ BusEnv ep cliID
--    liftIO $ print "closing"
--    return out


--incBus = do
--    x <- ask
--    return ()


--test :: BusMonad m => m ()
--test = do
--    liftIO $ print "dupa"
--    x <- ask
--    liftIO $ print x
--    incBus
--    incBus
--    incBus
--    incBus

--main = do
--    print =<< runBus test (BusEndPoints "tcp://127.0.0.1:30530" "" "")
