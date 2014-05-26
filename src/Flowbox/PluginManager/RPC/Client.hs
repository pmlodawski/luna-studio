---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.PluginManager.RPC.Client where

import Control.Monad.State
import Control.Monad.Trans.Either

import           Flowbox.Bus.Bus                                    (Bus)
import qualified Flowbox.Bus.RPC.Client                             as Client
import           Flowbox.PluginManager.Plugin.Plugin                (Plugin)
import qualified Flowbox.PluginManager.Plugin.Plugin                as Plugin
import           Flowbox.PluginManager.Prefix                       (Prefix)
import qualified Flowbox.PluginManager.Prefix                       as Prefix
import           Flowbox.PluginManager.Proto.Plugin                 ()
import qualified Flowbox.PluginManager.RPC.Topic                    as Topic
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger                          as L
import qualified Flowbox.Text.ProtocolBuffers                       as Proto
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.PluginManager.Plugin.Add.Request   as Add
import qualified Generated.Proto.PluginManager.Plugin.Add.Update    as Add
import qualified Generated.Proto.PluginManager.Plugin.Start.Request as Start
import qualified Generated.Proto.PluginManager.Plugin.Start.Update  as Start
--import qualified Generated.Proto.PluginManager.Plugin.List.Request    as List
--import qualified Generated.Proto.PluginManager.Plugin.List.Status     as List
--import qualified Generated.Proto.PluginManager.Plugin.Lookup.Request  as Lookup
--import qualified Generated.Proto.PluginManager.Plugin.Lookup.Status   as Lookup
--import qualified Generated.Proto.PluginManager.Plugin.Remove.Request  as Remove
--import qualified Generated.Proto.PluginManager.Plugin.Remove.Update   as Remove
--import qualified Generated.Proto.PluginManager.Plugin.Restart.Request as Restart
--import qualified Generated.Proto.PluginManager.Plugin.Restart.Update  as Restart
--import qualified Generated.Proto.PluginManager.Plugin.Stop.Request    as Stop
--import qualified Generated.Proto.PluginManager.Plugin.Stop.Update     as Stop



logger :: LoggerIO
logger = getLoggerIO "Flowbox.PluginManager.RPC.Client"




restrictSingleResult :: Proto.Serializable result
             => [result] -> Bus result
restrictSingleResult results = case results of
    [result] -> return result
    _        -> lift $ left  "RPC call failed: received multiple results"


add :: Prefix -> Plugin -> Bus Plugin.ID
add prefix plugin = do
    let topic = Prefix.prefixify prefix Topic.pluginAddRequest
    results <- Client.query topic $ Add.Request $ encode plugin
    decodeP . Add.id <$> restrictSingleResult results


start :: Prefix -> Plugin.ID -> Bus ()
start prefix pluginID = do
    let topic = Prefix.prefixify prefix Topic.pluginStartRequest
    results <- Client.query topic $ Start.Request $ encodeP pluginID
    void (restrictSingleResult results :: Bus Start.Update)


