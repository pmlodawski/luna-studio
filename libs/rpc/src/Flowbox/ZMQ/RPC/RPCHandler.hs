
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Flowbox.ZMQ.RPC.RPCHandler where

import           Data.ByteString                 (ByteString)
import           System.ZMQ4.Monadic             (ZMQ)
import qualified Text.ProtocolBuffers            as Proto
import qualified Text.ProtocolBuffers.Extensions as Extensions

import Flowbox.Prelude              hiding (error)
import Generated.Proto.Rpc.Response (Response)



type RPCHandler ctx request =
    forall z. (forall args result. (Show args, Show result)
    => (ctx -> args -> IO result)
    -> Extensions.Key Maybe request args
    -> Extensions.Key Maybe Response result -> ZMQ z ByteString)
    -> request -> ZMQ z ByteString


type ProtoSerializable request = (Proto.ReflectDescriptor request, Proto.Wire request)
