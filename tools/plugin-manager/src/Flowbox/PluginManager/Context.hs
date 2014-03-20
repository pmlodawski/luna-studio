---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.PluginManager.Context where

import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import           Data.Map   (Map)
import qualified Data.Map   as Map

import           Flowbox.Config.Config                   (Config)
import qualified Flowbox.PluginManager.Data.Plugin       as Plugin
import           Flowbox.PluginManager.Data.PluginHandle (PluginHandle)
import           Flowbox.Prelude                         hiding (Context)


type ContextRef = IORef Context


data Context = Context { config  :: Config
                       , plugins :: Map Plugin.ID PluginHandle
                       }


mk :: Config -> IO ContextRef
mk cfg = IORef.newIORef $ Context cfg Map.empty
