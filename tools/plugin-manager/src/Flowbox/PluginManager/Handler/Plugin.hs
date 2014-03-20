---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.PluginManager.Handler.Plugin where

import qualified Data.IORef as IORef

import           Flowbox.PluginManager.Context                      (ContextRef)
import           Flowbox.PluginManager.Proto.Plugin                 ()
import           Flowbox.Prelude                                    hiding (error)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.PluginManager.Plugin.List.Args     as List
import qualified Generated.Proto.PluginManager.Plugin.List.Result   as List
import qualified Generated.Proto.PluginManager.Plugin.Lookup.Args   as Lookup
import qualified Generated.Proto.PluginManager.Plugin.Lookup.Result as Lookup
import qualified Generated.Proto.PluginManager.Plugin.Start.Args    as Start
import qualified Generated.Proto.PluginManager.Plugin.Start.Result  as Start
import qualified Generated.Proto.PluginManager.Plugin.Stop.Args     as Stop
import qualified Generated.Proto.PluginManager.Plugin.Stop.Result   as Stop



logger :: LoggerIO
logger = getLoggerIO "Flowbox.PluginManager.Handler.Plugin"

-------- public api -------------------------------------------------

list :: ContextRef -> List.Args -> IO List.Result
list context List.Args = do
    logger error "Not implemented"
    return $ List.Result undefined


lookup :: ContextRef -> Lookup.Args -> IO Lookup.Result
lookup context (Lookup.Args tid) = do
    logger error "Not implemented"
    return $ Lookup.Result undefined


start :: ContextRef -> Start.Args -> IO Start.Result
start context (Start.Args tid) = do
    logger error "Not implemented"
    return Start.Result


stop :: ContextRef -> Stop.Args -> IO Stop.Result
stop context (Stop.Args tid) = do
    logger error "Not implemented"
    return Stop.Result
