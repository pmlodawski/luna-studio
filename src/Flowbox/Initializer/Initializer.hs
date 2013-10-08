---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Initializer.Initializer where

import           Control.Applicative
import           Control.Monad                        

import           Flowbox.Prelude                    hiding (error)
import           Flowbox.System.Log.Logger            
import qualified Flowbox.System.Directory.Directory as Directory
import qualified Flowbox.Initializer.Common         as Common
import qualified Flowbox.System.Process             as Process
import qualified Flowbox.System.UniPath             as UniPath
import           Flowbox.System.UniPath               (UniPath)



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Initializer.Initializer"


successfullInstallFileName :: UniPath
successfullInstallFileName = UniPath.append "installed" Common.flowboxPath


isAlreadyInitilized :: IO Bool
isAlreadyInitilized = do 
    logger debug "Checking for Flowbox configuration."
    exists_cabalDev  <- Directory.doesDirectoryExist $ UniPath.append "cabal-dev" Common.flowboxPath
    exists_installed <- Directory.doesFileExist successfullInstallFileName
    let exists = exists_cabalDev && exists_installed 
    if exists
        then logger debug "Configuration already exists."
        else logger debug "Configuration does not exist or is broken."
    return exists 


initializeIfNeeded :: IO ()
initializeIfNeeded = do
    initialized <- isAlreadyInitilized
    when (not initialized) (clear *> initialize)


initialize :: IO ()
initialize = do
    logger info "Configuring Flowbox for the first use. Please wait..."
    Directory.createDirectoryIfMissing True $ UniPath.append "tmp" Common.flowboxPath
    Process.runProcessInFolder Common.flowboxPath "cabal" ["update"] 
    Process.runProcessInFolder Common.flowboxPath "cabal" ["install", "cabal-dev"] 
    Process.runProcessInFolder Common.flowboxPath "cabal-dev" ["update"] 
    logger debug "Copying std library."
    Directory.copyDirectoryRecursive (UniPath.fromUnixString "libs/stdlibio/") (UniPath.append "tmp" Common.flowboxPath)
    logger debug "Intalling std library."
    let stdlibLocation = "tmp/stdlibio"
    Process.runProcessInFolder Common.flowboxPath "cabal-dev" ["install", stdlibLocation] 
    Directory.removeDirectoryRecursive $ UniPath.append stdlibLocation Common.flowboxPath
    Directory.touchFile successfullInstallFileName
    logger info "Flowbox configured successfully."


clear :: IO ()
clear = do 
    logger info "Cleaning Flowbox configuration."
    exists <- Directory.doesDirectoryExist Common.flowboxPath
    if exists
        then Directory.removeDirectoryRecursive Common.flowboxPath
        else return ()