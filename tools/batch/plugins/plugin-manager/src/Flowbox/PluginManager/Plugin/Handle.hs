---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.PluginManager.Plugin.Handle where

import           System.Process (ProcessHandle)
import qualified System.Process as Process

import           Flowbox.PluginManager.Plugin.Info   (PluginInfo (PluginInfo))
import qualified Flowbox.PluginManager.Plugin.Info   as PluginInfo
import           Flowbox.PluginManager.Plugin.Plugin (Plugin)
import qualified Flowbox.PluginManager.Plugin.Plugin as Plugin
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger           as L



logger :: LoggerIO
logger = getLoggerIO "Flowbox.PluginManager.Data.PluginHandle"


data PluginHandle = PluginHandle { plugin :: Plugin
                                 , handle :: Maybe ProcessHandle
                                 }

mk :: Plugin -> PluginHandle
mk = flip PluginHandle Nothing


info :: PluginHandle -> IO PluginInfo
info ph = PluginInfo (plugin ph) <$> case handle ph of
    Nothing -> return PluginInfo.Stopped
    Just h  -> do exitCode <- Process.getProcessExitCode h
                  case exitCode of
                     Just _  -> return PluginInfo.Stopped
                     Nothing -> return PluginInfo.Running


start :: Plugin -> IO PluginHandle
start p = do logger L.info $ "Starting plugin " ++ (show $ Plugin.name p) ++ " (" ++ Plugin.command p ++ ")"
             -- FIXME [PM] : handle fail to start
             h <- Process.spawnCommand $ Plugin.command p
             return $ PluginHandle p $ Just h


stop :: PluginHandle -> IO PluginHandle
stop ph = do logger L.info $ "Stopping plugin " ++ (show $ Plugin.name $ plugin ph)
             case handle ph of
                Just h  -> do Process.terminateProcess h
                              _ <- Process.waitForProcess h
                              logger L.info "Plugin stopped"
                Nothing -> logger L.info "No need to stop. Plugin already stopped"
             return $ ph { handle = Nothing }


restart :: PluginHandle -> IO PluginHandle
restart ph = plugin <$> stop ph >>= start
