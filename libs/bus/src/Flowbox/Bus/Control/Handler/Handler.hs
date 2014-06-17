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
import Flowbox.ZMQ.RPC.Handler        (RPCHandler)

import qualified Generated.Proto.Bus.ID.Create.Args   as ID_Create
import qualified Generated.Proto.Bus.ID.Create.Result as ID_Create
import           Generated.Proto.Bus.Request          (Request)
import qualified Generated.Proto.Bus.Request          as Request
import qualified Generated.Proto.Bus.Request.Method   as Method



handler :: BusCtx -> RPCHandler Request
handler ctx callback request = case Request.method request of
    Method.ID_Create -> callback (HandlerID.create ctx) ID_Create.req ID_Create.rsp
