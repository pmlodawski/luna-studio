---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Initializer.Initializer where

import           Control.Monad                        

import           Flowbox.Prelude                    hiding (error)
import           Flowbox.System.Log.Logger            
import qualified Flowbox.System.Directory.Directory as Directory
import qualified Flowbox.Initializer.Common         as Common
import qualified Flowbox.System.Process             as Process
import qualified Flowbox.System.UniPath             as UniPath
import           Flowbox.System.UniPath               (UniPath)



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Initializer.Initializer"


installedFile :: UniPath
installedFile = UniPath.append "installed" Common.flowboxPath


isAlreadyInitilized :: IO Bool
isAlreadyInitilized = do 
    loggerIO debug "Checking for Flowbox configuration." 
    exists_cabalDev  <- Directory.doesDirectoryExist $ UniPath.append "cabal-dev" Common.flowboxPath
    exists_installed <- Directory.doesFileExist installedFile
    let exists = exists_cabalDev && exists_installed
    if exists
        then loggerIO debug "Configuration already exists."
        else loggerIO debug "Configuration does not exist or is broken."
    return exists


checkedInitialize :: IO ()
checkedInitialize = do
    initialized <- isAlreadyInitilized
    when (not initialized) (do clear
                               initialize)


initialize :: IO ()
initialize = do
    loggerIO info "Configuring Flowbox for the first use. Please wait..."
    Directory.createDirectoryIfMissing True $ UniPath.append "tmp" Common.flowboxPath
    Process.runProcessInFolder Common.flowboxPath "cabal" ["update"] 
    Process.runProcessInFolder Common.flowboxPath "cabal" ["install", "cabal-dev"] 
    Process.runProcessInFolder Common.flowboxPath "cabal-dev" ["update"] 
    loggerIO debug "Copying std library."
    Directory.copyDirectoryRecursive (UniPath.fromUnixString "libs/stdlibio/") (UniPath.append "tmp" Common.flowboxPath)
    loggerIO debug "Intalling std library."
    let location = "tmp/stdlibio"
    Process.runProcessInFolder Common.flowboxPath "cabal-dev" ["install", location] 
    Directory.removeDirectoryRecursive $ UniPath.append location Common.flowboxPath
    Directory.touchFile installedFile
    loggerIO info "Flowbox configured successfully."


clear :: IO ()
clear = do 
    loggerIO info "Cleaning Flowbox configuration."
    exists <- Directory.doesDirectoryExist Common.flowboxPath
    if exists
        then Directory.removeDirectoryRecursive Common.flowboxPath
        else return ()