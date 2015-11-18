---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Bus.RPC.Server.Server where

import           Flowbox.Bus.Bus                  (Bus)
import           Flowbox.Bus.EndPoint             (BusEndPoints)
import           Flowbox.Bus.RPC.HandlerMap       (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap       as HandlerMap
import qualified Flowbox.Bus.RPC.Server.Processor as Processor
import qualified Flowbox.Bus.Server               as Server
import qualified Flowbox.Bus.Util                 as Util
import           Flowbox.Prelude                  hiding (error)
import           Flowbox.System.Log.Logger

logger :: LoggerIO
logger = getLoggerIO $moduleName


run :: String -> Bus () -> BusEndPoints -> s -> HandlerMap s IO -> IO (Either String ())
run pluginName initialize endPoints s handlerMap =
    Server.runState initialize endPoints (HandlerMap.topics pluginName handlerMap) s $ Processor.process handlerMap


runWithInit :: String -> Bus () -> BusEndPoints -> s -> HandlerMap s IO -> IO (Either String ())
runWithInit pluginName initialize endPoints s handlerMap =
    Server.runState initialize endPoints (HandlerMap.topics pluginName handlerMap) s $ Processor.process handlerMap


runDetectDuplicate :: String -> Util.Ping -> BusEndPoints -> s -> HandlerMap s IO -> IO (Either String ())
runDetectDuplicate pluginName ping = runWithInit pluginName (Util.quitIfExists pluginName ping)
