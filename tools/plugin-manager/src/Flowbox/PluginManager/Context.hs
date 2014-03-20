---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.PluginManager.Context where

import           Data.IORef (IORef)
import qualified Data.IORef as IORef

import           Flowbox.Config.Config                (Config)
import           Flowbox.PluginManager.Data.PluginMap (PluginMap)
import qualified Flowbox.PluginManager.Data.PluginMap as PluginMap
import           Flowbox.Prelude                      hiding (Context)



type ContextRef = IORef Context


data Context = Context { config  :: Config
                       , plugins :: PluginMap
                       }


mk :: Config -> IO ContextRef
mk cfg = IORef.newIORef $ Context cfg PluginMap.empty

