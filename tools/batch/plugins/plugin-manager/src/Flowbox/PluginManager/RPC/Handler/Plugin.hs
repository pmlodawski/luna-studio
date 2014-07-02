---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.PluginManager.RPC.Handler.Plugin where

import qualified Data.IORef as IORef

import           Flowbox.Bus.RPC.RPC                                  (RPC)
import           Flowbox.Control.Error
import           Flowbox.PluginManager.Context                        (ContextRef)
import qualified Flowbox.PluginManager.Context                        as Context
import           Flowbox.PluginManager.Plugin.Handle                  (PluginHandle)
import qualified Flowbox.PluginManager.Plugin.Handle                  as PluginHandle
import qualified Flowbox.PluginManager.Plugin.Map                     as PluginMap
import qualified Flowbox.PluginManager.Plugin.Plugin                  as Plugin
import           Flowbox.PluginManager.Proto.Plugin                   ()
import           Flowbox.Prelude                                      hiding (error, id)
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

add :: ContextRef -> Add.Request -> RPC Add.Update
add ctxRef (Add.Request tplugin) = do
    ctx <- liftIO $ IORef.readIORef ctxRef
    let plugins = Context.plugins ctx
        id      = PluginMap.uniqueID plugins
    plugin <- decodeE tplugin
    liftIO $ IORef.writeIORef ctxRef ctx { Context.plugins = PluginMap.insert id (PluginHandle.mk plugin) plugins}
    return $ Add.Update tplugin (encodeP id)


remove :: ContextRef -> Remove.Request -> RPC Remove.Update
remove ctxRef (Remove.Request tid) = safeLiftIO $ do
    ctx <- IORef.readIORef ctxRef
    let id      = decodeP tid
        plugins = Context.plugins ctx
    IORef.writeIORef ctxRef ctx { Context.plugins = PluginMap.delete id plugins}
    return $ Remove.Update tid


list :: ContextRef -> List.Request -> RPC List.Status
list ctxRef List.Request = safeLiftIO $ do
    ctx <- IORef.readIORef ctxRef
    let plugins = Context.plugins ctx
    pluginInfos <- mapM PluginHandle.info $ PluginMap.elems plugins
    return $ List.Status (encodeList $ zip (PluginMap.keys plugins) pluginInfos)


-- TODO [PM] : Duplikacja kodu
lookup :: ContextRef -> Lookup.Request -> RPC Lookup.Status
lookup ctxRef (Lookup.Request tid) = do
    ctx <- liftIO $ IORef.readIORef ctxRef
    let id      = decodeP tid
        plugins = Context.plugins ctx
    pluginHandle <- PluginMap.lookup id plugins <??> "Cannot find plugin with id=" ++ show id
    pluginInfo   <- safeLiftIO $ PluginHandle.info pluginHandle
    return $ Lookup.Status (encode (id, pluginInfo)) tid


start :: ContextRef -> Start.Request -> RPC Start.Update
start ctxRef (Start.Request tid) = do
    let id = decodeP tid
    _ <- withPluginHandle ctxRef id (PluginHandle.start . view PluginHandle.plugin)
    return $ Start.Update tid


stop :: ContextRef -> Stop.Request -> RPC Stop.Update
stop ctxRef (Stop.Request tid) = do
    let id = decodeP tid
    _ <- withPluginHandle ctxRef id PluginHandle.stop
    return $ Stop.Update tid


restart :: ContextRef -> Restart.Request -> RPC Restart.Update
restart ctxRef (Restart.Request tid) = do
    let id = decodeP tid
    _ <- withPluginHandle ctxRef id PluginHandle.restart
    return $ Restart.Update tid


withPluginHandle :: ContextRef -> Plugin.ID -> (PluginHandle -> IO PluginHandle) -> RPC PluginHandle
withPluginHandle ctxRef id operation = do
    ctx <- liftIO $ IORef.readIORef ctxRef
    let plugins = Context.plugins ctx
    pluginHandle    <- PluginMap.lookup id plugins <??> "Cannot find plugin with id=" ++ show id
    newPluginHandle <- safeLiftIO $ operation pluginHandle
    liftIO $ IORef.writeIORef ctxRef ctx { Context.plugins = PluginMap.insert id newPluginHandle plugins}
    return newPluginHandle
