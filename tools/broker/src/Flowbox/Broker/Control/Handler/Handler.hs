
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Broker.Control.Handler.Handler where

import Flowbox.Broker.Control.BrokerCtx  (BrokerCtx)
import Flowbox.Broker.Control.Handler.ID as HandlerID
import Flowbox.ZMQ.RPC.RPCHandler        (RPCHandler)

import qualified Generated.Proto.Broker.ID.New.Args    as ID_New
import qualified Generated.Proto.Broker.ID.New.Result  as ID_New
import           Generated.Proto.Broker.Request        (Request)
import qualified Generated.Proto.Broker.Request        as Request
import qualified Generated.Proto.Broker.Request.Method as Method



handler :: RPCHandler BrokerCtx Request
handler callback request = case Request.method request of
    Method.ID_New  -> callback HandlerID.new  ID_New.req  ID_New.rsp
