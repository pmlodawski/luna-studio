
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Bus.Control.Handler.Handler where

import Flowbox.Bus.Control.BusCtx     (BusCtx)
import Flowbox.Bus.Control.Handler.ID as HandlerID
import Flowbox.ZMQ.RPC.RPCHandler     (RPCHandler)

import qualified Generated.Proto.Bus.ID.New.Args    as ID_New
import qualified Generated.Proto.Bus.ID.New.Result  as ID_New
import           Generated.Proto.Bus.Request        (Request)
import qualified Generated.Proto.Bus.Request        as Request
import qualified Generated.Proto.Bus.Request.Method as Method



handler :: BusCtx -> RPCHandler Request
handler ctx callback request = case Request.method request of
    Method.ID_New -> callback (HandlerID.new ctx) ID_New.req ID_New.rsp
