---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.PluginManager.Data.PluginInfo where

import Flowbox.PluginManager.Data.Plugin (Plugin)
import Flowbox.Prelude



data Status = Running
            | Stopped
            deriving (Read, Show)


data PluginInfo = PluginInfo { plugin :: Plugin
                             , status :: Status
                             } deriving (Read, Show)
