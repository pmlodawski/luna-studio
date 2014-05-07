---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.PluginManager.Init where

import           Control.Monad.Trans.Either
import qualified Data.Configurator          as Configurator
import           Data.Configurator.Types    (Name, Value)
import qualified Data.Configurator.Types    as Configurator
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text                  as Text

import           Flowbox.Control.Error                   (safeLiftIO)
import           Flowbox.PluginManager.Data.Plugin       (Plugin (Plugin))
import           Flowbox.PluginManager.Data.PluginHandle (PluginHandle)
import qualified Flowbox.PluginManager.Data.PluginHandle as PluginHandle
import           Flowbox.Prelude



readPlugins :: FilePath -> EitherT String IO [Plugin]
readPlugins filePath = do
    cfg    <- safeLiftIO $ Configurator.load [Configurator.Required filePath]
    cfgMap <- safeLiftIO $ Configurator.getMap cfg
    mapM (hoistEither . readPlugin) $ HashMap.toList cfgMap


readPlugin :: (Name, Value) -> Either String Plugin
readPlugin (name, value) = case value of
    Configurator.String command -> Right $ Plugin (Text.unpack name) (Text.unpack command)
    _                           -> Left  $ "Cannot parse command for " ++ show name ++ " in config file."



init :: FilePath -> EitherT String IO [PluginHandle]
init filePath = do
    plugins <- readPlugins filePath
    safeLiftIO $ mapM PluginHandle.start plugins
