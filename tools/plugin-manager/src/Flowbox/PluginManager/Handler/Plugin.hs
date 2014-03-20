---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.PluginManager.Handler.Plugin where

import qualified Data.IORef                           as IORef
import qualified Flowbox.PluginManager.Data.PluginMap as PluginMap

import           Flowbox.Control.Error
import           Flowbox.PluginManager.Context                      (ContextRef)
import qualified Flowbox.PluginManager.Context                      as Context
import qualified Flowbox.PluginManager.Data.Plugin                  as Plugin
import           Flowbox.PluginManager.Data.PluginHandle            (PluginHandle)
import qualified Flowbox.PluginManager.Data.PluginHandle            as PluginHandle
import           Flowbox.PluginManager.Proto.Plugin                 ()
import           Flowbox.Prelude                                    hiding (error, id)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.PluginManager.Plugin.Add.Args      as Add
import qualified Generated.Proto.PluginManager.Plugin.Add.Result    as Add
import qualified Generated.Proto.PluginManager.Plugin.List.Args     as List
import qualified Generated.Proto.PluginManager.Plugin.List.Result   as List
import qualified Generated.Proto.PluginManager.Plugin.Lookup.Args   as Lookup
import qualified Generated.Proto.PluginManager.Plugin.Lookup.Result as Lookup
import qualified Generated.Proto.PluginManager.Plugin.Remove.Args   as Remove
import qualified Generated.Proto.PluginManager.Plugin.Remove.Result as Remove
import qualified Generated.Proto.PluginManager.Plugin.Start.Args    as Start
import qualified Generated.Proto.PluginManager.Plugin.Start.Result  as Start
import qualified Generated.Proto.PluginManager.Plugin.Stop.Args     as Stop
import qualified Generated.Proto.PluginManager.Plugin.Stop.Result   as Stop



logger :: LoggerIO
logger = getLoggerIO "Flowbox.PluginManager.Handler.Plugin"

-------- public api -------------------------------------------------

add :: ContextRef -> Add.Args -> IO Add.Result
add ctxRef (Add.Args tplugin) = do
    ctx <- IORef.readIORef ctxRef
    let plugins = Context.plugins ctx
        id      = PluginMap.uniqueID plugins
    plugin <- decode tplugin
    IORef.writeIORef ctxRef ctx { Context.plugins = PluginMap.insert id (PluginHandle.mk plugin) plugins}
    return $ Add.Result $ encodeP id


remove :: ContextRef -> Remove.Args -> IO Remove.Result
remove ctxRef (Remove.Args tid) = do
    ctx <- IORef.readIORef ctxRef
    let id      = decodeP tid
        plugins = Context.plugins ctx
    IORef.writeIORef ctxRef ctx { Context.plugins = PluginMap.delete id plugins}
    return Remove.Result


list :: ContextRef -> List.Args -> IO List.Result
list ctxRef List.Args = do
    ctx <- IORef.readIORef ctxRef
    let plugins = Context.plugins ctx
    pluginInfos <- mapM PluginHandle.info $ PluginMap.elems plugins
    return $ List.Result $ encodeList $ zip (PluginMap.keys plugins) pluginInfos


-- TODO [PM] : Duplikacja kodu
lookup :: ContextRef -> Lookup.Args -> IO Lookup.Result
lookup ctxRef (Lookup.Args tid) = do
    ctx <- IORef.readIORef ctxRef
    let id      = decodeP tid
        plugins = Context.plugins ctx
    pluginHandle <- PluginMap.lookup id plugins <?> "Cannot find plugin with id=" ++ show id
    pluginInfo   <- PluginHandle.info pluginHandle
    return $ Lookup.Result $ encode (id, pluginInfo)


start :: ContextRef -> Start.Args -> IO Start.Result
start ctxRef (Start.Args tid) = do
    let id = decodeP tid
    _ <- withPluginHandle ctxRef id (PluginHandle.start . PluginHandle.plugin)
    return Start.Result


stop :: ContextRef -> Stop.Args -> IO Stop.Result
stop ctxRef (Stop.Args tid) = do
    let id = decodeP tid
    _ <- withPluginHandle ctxRef id PluginHandle.stop
    return Stop.Result


withPluginHandle :: ContextRef -> Plugin.ID -> (PluginHandle -> IO PluginHandle) -> IO PluginHandle
withPluginHandle ctxRef id operation = do
    ctx <- IORef.readIORef ctxRef
    let plugins = Context.plugins ctx
    pluginHandle    <- PluginMap.lookup id plugins <?> "Cannot find plugin with id=" ++ show id
    newPluginHandle <- operation pluginHandle
    IORef.writeIORef ctxRef ctx { Context.plugins = PluginMap.insert id newPluginHandle plugins}
    return newPluginHandle
