---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.PluginManager.Context where

import           Data.IORef (IORef)
import qualified Data.IORef as IORef

import           Flowbox.Config.Config               (Config)
import           Flowbox.PluginManager.Plugin.Handle (PluginHandle)
import           Flowbox.PluginManager.Plugin.Map    (PluginMap)
import qualified Flowbox.PluginManager.Plugin.Map    as PluginMap
import           Flowbox.Prelude                     hiding (Context)



data Context = Context { config  :: Config
                       , plugins :: PluginMap
                       }


mk :: Config -> [PluginHandle] -> Context
mk cfg pluginHandles = Context cfg $ PluginMap.fromList $ zip [0..] pluginHandles

