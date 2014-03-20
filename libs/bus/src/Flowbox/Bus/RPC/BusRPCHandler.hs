---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Bus.RPC.BusRPCHandler where

import           Flowbox.Bus.Message          (Message)
import           Flowbox.Bus.Topic            (Topic)
import           Flowbox.Prelude
import qualified Flowbox.Text.ProtocolBuffers as Proto


type BusRPCHandler = (forall args result. (Proto.Serializable args, Proto.Serializable result)
                       => String -> (args -> IO result) -> IO Message)
                   -> Topic -> IO Message
