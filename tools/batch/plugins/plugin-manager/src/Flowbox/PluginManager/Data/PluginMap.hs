---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.PluginManager.Data.PluginMap (
    module Data.IntMap,
    PluginMap,
    uniqueID,
) where

import           Data.IntMap
import qualified Data.IntMap as IntMap

import qualified Flowbox.PluginManager.Data.Plugin       as Plugin
import           Flowbox.PluginManager.Data.PluginHandle (PluginHandle)
import           Flowbox.Prelude



type PluginMap = IntMap PluginHandle


uniqueID :: PluginMap -> Plugin.ID
uniqueID pluginMap = if IntMap.null pluginMap
    then 0
    else ((+) 1 . fst . IntMap.findMax) pluginMap
