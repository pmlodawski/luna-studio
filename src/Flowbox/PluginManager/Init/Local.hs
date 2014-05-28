---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Flowbox.PluginManager.Init.Local where

import           Control.Arrow              (first)
import           Control.Monad.Trans.Either
import qualified Data.Configurator          as Configurator
import           Data.Configurator.Types    (Name, Value)
import qualified Data.Configurator.Types    as Configurator
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text                  as Text

import           Flowbox.Control.Error               (safeLiftIO)
import           Flowbox.PluginManager.Plugin.Handle (PluginHandle)
import qualified Flowbox.PluginManager.Plugin.Handle as PluginHandle
import           Flowbox.PluginManager.Plugin.Plugin (Plugin (Plugin))
import           Flowbox.Prelude



localSection :: Name
localSection = "plugins."


isPluginRecord :: (Name, Value) -> Bool
isPluginRecord (name, _) = Text.isPrefixOf localSection name


dropPluginPrefix :: Name -> Name
dropPluginPrefix = Text.drop $ Text.length localSection


readPlugins :: FilePath -> EitherT String IO [Plugin]
readPlugins filePath = do
    cfg    <- safeLiftIO $ Configurator.load [Configurator.Required filePath]
    cfgMap <- safeLiftIO $ Configurator.getMap cfg
    mapM (hoistEither . readPlugin) $ map (first dropPluginPrefix)
                                    $ filter isPluginRecord
                                    $ HashMap.toList cfgMap


readPlugin :: (Name, Value) -> Either String Plugin
readPlugin (name, value) = case value of
    Configurator.String command -> Right $ Plugin (Text.unpack name) (Text.unpack command)
    _                           -> Left  $ "Cannot parse command for " ++ show name ++ " in config file."


init :: FilePath -> EitherT String IO [PluginHandle]
init filePath = do
    plugins <- readPlugins filePath
    safeLiftIO $ mapM PluginHandle.start plugins
