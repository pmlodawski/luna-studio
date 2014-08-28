---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.PluginManager.RPC.Handler.Plugin where

import Control.Monad.Trans.State

import           Flowbox.Bus.RPC.RPC                                  (RPC)
import           Flowbox.Control.Error
import           Flowbox.PluginManager.Context                        (Context)
import qualified Flowbox.PluginManager.Context                        as Context
import           Flowbox.PluginManager.Plugin.Handle                  (PluginHandle)
import qualified Flowbox.PluginManager.Plugin.Handle                  as PluginHandle
import qualified Flowbox.PluginManager.Plugin.Map                     as PluginMap
import qualified Flowbox.PluginManager.Plugin.Plugin                  as Plugin
import           Flowbox.PluginManager.Proto.Plugin                   ()
import           Flowbox.Prelude                                      hiding (Context, error, id)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.PluginManager.Plugin.Add.Request     as Add
import qualified Generated.Proto.PluginManager.Plugin.Add.Update      as Add
import qualified Generated.Proto.PluginManager.Plugin.List.Request    as List
import qualified Generated.Proto.PluginManager.Plugin.List.Status     as List
import qualified Generated.Proto.PluginManager.Plugin.Lookup.Request  as Lookup
import qualified Generated.Proto.PluginManager.Plugin.Lookup.Status   as Lookup
import qualified Generated.Proto.PluginManager.Plugin.Remove.Request  as Remove
import qualified Generated.Proto.PluginManager.Plugin.Remove.Update   as Remove
import qualified Generated.Proto.PluginManager.Plugin.Restart.Request as Restart
import qualified Generated.Proto.PluginManager.Plugin.Restart.Update  as Restart
import qualified Generated.Proto.PluginManager.Plugin.Start.Request   as Start
import qualified Generated.Proto.PluginManager.Plugin.Start.Update    as Start
import qualified Generated.Proto.PluginManager.Plugin.Stop.Request    as Stop
import qualified Generated.Proto.PluginManager.Plugin.Stop.Update     as Stop



logger :: LoggerIO
logger = getLoggerIO "Flowbox.PluginManager.RPC.Handler.Plugin"

-------- public api -------------------------------------------------

add :: Add.Request -> RPC Context IO Add.Update
add (Add.Request tplugin) = do
    ctx <- lift get
    let plugins = Context.plugins ctx
        id      = PluginMap.uniqueID plugins
    plugin <- decodeE tplugin
    lift $ put $ ctx { Context.plugins = PluginMap.insert id (PluginHandle.mk plugin) plugins}
    return $ Add.Update tplugin (encodeP id)


remove :: Remove.Request -> RPC Context IO Remove.Update
remove (Remove.Request tid) = do
    ctx <- lift get
    let id      = decodeP tid
        plugins = Context.plugins ctx
    lift $ put $ ctx { Context.plugins = PluginMap.delete id plugins}
    return $ Remove.Update tid


list :: List.Request -> RPC Context IO List.Status
list List.Request = do
    ctx <- lift get
    let plugins = Context.plugins ctx
    pluginInfos <- safeLiftIO $ mapM PluginHandle.info $ PluginMap.elems plugins
    return $ List.Status (encodeList $ zip (PluginMap.keys plugins) pluginInfos)


-- TODO [PM] : Duplikacja kodu
lookup :: Lookup.Request -> RPC Context IO Lookup.Status
lookup (Lookup.Request tid) = do
    ctx <- lift get
    let id      = decodeP tid
        plugins = Context.plugins ctx
    pluginHandle <- PluginMap.lookup id plugins <??> "Cannot find plugin with id=" ++ show id
    pluginInfo   <- safeLiftIO $ PluginHandle.info pluginHandle
    return $ Lookup.Status (encode (id, pluginInfo)) tid


start :: Start.Request -> RPC Context IO Start.Update
start (Start.Request tid) = do
    let id = decodeP tid
    _ <- withPluginHandle id (PluginHandle.start . view PluginHandle.plugin)
    return $ Start.Update tid


stop :: Stop.Request -> RPC Context IO Stop.Update
stop (Stop.Request tid) = do
    let id = decodeP tid
    _ <- withPluginHandle id PluginHandle.stop
    return $ Stop.Update tid


restart :: Restart.Request -> RPC Context IO Restart.Update
restart (Restart.Request tid) = do
    let id = decodeP tid
    _ <- withPluginHandle id PluginHandle.restart
    return $ Restart.Update tid


withPluginHandle :: Plugin.ID -> (PluginHandle -> IO PluginHandle) -> RPC Context IO PluginHandle
withPluginHandle id operation = do
    ctx <- lift get
    let plugins = Context.plugins ctx
    pluginHandle    <- PluginMap.lookup id plugins <??> "Cannot find plugin with id=" ++ show id
    newPluginHandle <- safeLiftIO $ operation pluginHandle
    lift $ put $ ctx { Context.plugins = PluginMap.insert id newPluginHandle plugins}
    return newPluginHandle
