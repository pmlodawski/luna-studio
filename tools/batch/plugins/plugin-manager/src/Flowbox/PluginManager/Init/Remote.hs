---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.PluginManager.Init.Remote where

import           Control.Monad.Trans.Either
import qualified Data.Configurator          as Configurator
import           Data.Configurator.Types    (Name, Value)
import qualified Data.Configurator.Types    as Configurator
import           Data.Either                (rights)
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.String.Utils          as Utils
import qualified Data.Text                  as Text

import           Flowbox.Bus.Bus                     (Bus)
import qualified Flowbox.Bus.Bus                     as Bus
import           Flowbox.Bus.Data.Prefix             (Prefix)
import           Flowbox.Bus.EndPoint                (BusEndPoints)
import           Flowbox.Control.Error               (safeLiftIO)
import qualified Flowbox.PluginManager.Plugin.Info   as PluginInfo
import           Flowbox.PluginManager.Plugin.Plugin (Plugin (Plugin))
import qualified Flowbox.PluginManager.Plugin.Plugin as Plugin
import qualified Flowbox.PluginManager.RPC.Client    as Client
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


type Error = String


data PluginKey = PluginKey { prefix :: Prefix
                           , plugin :: Plugin
                           } deriving (Show, Read)


parseKey :: (Name, Value) -> Either Error PluginKey
parseKey (key, value) = case Utils.split "." $ Text.unpack key of
    ["remote", prefix', name] -> case value of
        Configurator.String command -> Right $ PluginKey prefix' $ Plugin name $ Text.unpack command
        _                           -> Left  $ "Cannot parse command for " ++ show name
    _                         -> Left  $ "Cannot parse plugin " ++ show key


readPlugins :: FilePath -> EitherT Error IO [PluginKey]
readPlugins filePath = do
    cfg    <- safeLiftIO $ Configurator.load [Configurator.Required filePath]
    cfgMap <- safeLiftIO $ Configurator.getMap cfg
    return $ rights $ map parseKey $ HashMap.toList cfgMap


installPlugin :: PluginKey -> Bus ()
installPlugin pluginKey = do
    let prefix'    = prefix pluginKey
        plugin'    = plugin pluginKey
        pluginName = plugin' ^. Plugin.name
    logger info $ "Installing " ++ show pluginName ++ " on " ++ show prefix' ++ "..."
    Client.waitForPingReply prefix'
    remoteList <- Client.list prefix'
    if plugin' `elem` map (view PluginInfo.plugin) remoteList
        then logger info $ "Plugin " ++ show pluginName ++ " already installed on " ++ show prefix' ++ "."
        else do pluginID <- Client.add prefix' $ plugin pluginKey
                Client.start prefix' pluginID
                logger info $ "Installed " ++ show pluginName ++ " on " ++ show prefix' ++ "."


init :: FilePath -> BusEndPoints -> EitherT Error IO ()
init filePath endPoints = do
    plugins <- readPlugins filePath
    logger info $ "Loaded " ++ show (length plugins) ++ " remote plugins to install."
    EitherT $ Bus.runBus endPoints $ mapM_ installPlugin plugins
