---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.PluginManager.RPC.Topic where



pluginAddRequest     = "plugin.add.request"
pluginRemoveRequest  = "plugin.remove.request"
pluginListRequest    = "plugin.list.request"
pluginLookupRequest  = "plugin.lookup.request"
pluginStartRequest   = "plugin.start.request"
pluginStopRequest    = "plugin.stop.request"
pluginRestartRequest = "plugin.restart.request"
pluginManagerPingRequest = "pluginmanager.ping.request"
