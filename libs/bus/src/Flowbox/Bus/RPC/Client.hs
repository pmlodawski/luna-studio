---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Bus.RPC.Client where

import qualified Flowbox.Bus.Client            as Client
import           Flowbox.Bus.EndPoint          (BusEndPoints)
import           Flowbox.Bus.RPC.BusRPCHandler (BusRPCHandler)
import qualified Flowbox.Bus.RPC.Processor     as Processor
import           Flowbox.Bus.Topic             (Topic)
import           Flowbox.Prelude               hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.RPC.Client"


run :: BusEndPoints -> [Topic] -> BusRPCHandler -> IO (Either String ())
run endPoints topics handler = Client.run endPoints topics $ Processor.process handler
