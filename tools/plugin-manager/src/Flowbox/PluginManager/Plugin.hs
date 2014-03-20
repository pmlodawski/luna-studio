---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.PluginManager.Plugin where

import Flowbox.Prelude



data Plugin = Plugin { name    :: String
                     , command :: String
                     } deriving (Read, Show)


data Status = Running
            | Stopped
            deriving (Read, Show)


data PluginInfo = PluginInfo { id     :: Int
                             , plugin :: Plugin
                             , status :: Status
                             } deriving (Read, Show)
