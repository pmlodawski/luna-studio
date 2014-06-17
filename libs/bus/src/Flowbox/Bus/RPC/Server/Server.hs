---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Bus.RPC.Server.Server where

import           Flowbox.Bus.EndPoint             (BusEndPoints)
import           Flowbox.Bus.RPC.HandlerMap       (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap       as HandlerMap
import qualified Flowbox.Bus.RPC.Server.Processor as Processor
import qualified Flowbox.Bus.Server               as Server
import           Flowbox.Prelude                  hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.RPC.Server.Server"


run :: BusEndPoints -> HandlerMap -> IO (Either String ())
run endPoints handlerMap = Server.run endPoints (HandlerMap.topics handlerMap) $ Processor.process handlerMap

