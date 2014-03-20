---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.PluginManager.Data.PluginHandle where

import           System.Process (ProcessHandle)
import qualified System.Process as Process

import           Flowbox.PluginManager.Data.Plugin     (Plugin)
import qualified Flowbox.PluginManager.Data.Plugin     as Plugin
import           Flowbox.PluginManager.Data.PluginInfo (PluginInfo (PluginInfo))
import qualified Flowbox.PluginManager.Data.PluginInfo as PluginInfo
import           Flowbox.Prelude



data PluginHandle = PluginHandle { plugin :: Plugin
                                 , handle :: Maybe ProcessHandle
                                 }

info :: PluginHandle -> IO PluginInfo
info ph = PluginInfo (plugin ph) <$> case handle ph of
    Nothing -> return PluginInfo.Stopped
    Just h  -> do exitCode <- Process.getProcessExitCode h
                  case exitCode of
                     Just _  -> return PluginInfo.Stopped
                     Nothing -> return PluginInfo.Running


start :: Plugin -> IO PluginHandle
start p = do h <- Process.runCommand (Plugin.command p)
             return $ PluginHandle p $ Just h


stop :: PluginHandle -> IO PluginHandle
stop ph = do case handle ph of
                Just h  -> Process.terminateProcess h
                Nothing -> return ()
             return $ ph { handle = Nothing }
