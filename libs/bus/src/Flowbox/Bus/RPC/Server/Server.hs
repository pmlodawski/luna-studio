---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Bus.RPC.Server.Server where

import           Flowbox.Bus.Data.Topic           (Topic)
import           Flowbox.Bus.EndPoint             (BusEndPoints)
import           Flowbox.Bus.RPC.Handler          (BusRPCHandler)
import qualified Flowbox.Bus.RPC.Server.Processor as Processor
import qualified Flowbox.Bus.Server               as Server
import           Flowbox.Prelude                  hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.RPC.Server.Server"


run :: BusEndPoints -> [Topic] -> BusRPCHandler -> IO (Either String ())
run endPoints topics handler = Server.run endPoints topics $ Processor.process handler
