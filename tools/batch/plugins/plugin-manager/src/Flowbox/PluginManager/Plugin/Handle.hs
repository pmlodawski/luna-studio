---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

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


data PluginHandle = PluginHandle { _plugin :: Plugin
                                 , _handle :: Maybe ProcessHandle
                                 }

makeLenses (''PluginHandle)


mk :: Plugin -> PluginHandle
mk = flip PluginHandle Nothing


info :: PluginHandle -> IO PluginInfo
info ph = PluginInfo (ph ^. plugin) <$> case ph ^. handle of
    Nothing -> return PluginInfo.Stopped
    Just h  -> do exitCode <- Process.getProcessExitCode h
                  case exitCode of
                     Just _  -> return PluginInfo.Stopped
                     Nothing -> return PluginInfo.Running


start :: Plugin -> IO PluginHandle
start p = do logger L.info $ "Starting plugin "
                          ++ (show $ p ^. Plugin.name)
                          ++ " (" ++ (p ^. Plugin.command) ++ ")"
             -- FIXME [PM] : handle fail to start
             h <- Process.spawnCommand $ p ^. Plugin.command
             return $ PluginHandle p $ Just h


stop :: PluginHandle -> IO PluginHandle
stop ph = do logger L.info $ "Stopping plugin " ++ (show $ ph ^. plugin . Plugin.name)
             case ph ^. handle of
                Just h  -> do Process.terminateProcess h
                              _ <- Process.waitForProcess h
                              logger L.info "Plugin stopped"
                Nothing -> logger L.info "No need to stop. Plugin already stopped"
             return $ ph & handle .~ Nothing


restart :: PluginHandle -> IO PluginHandle
restart ph = view plugin <$> stop ph >>= start
