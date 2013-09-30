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



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Initializer.Initializer"


isAlreadyInitilized :: IO Bool
isAlreadyInitilized = do 
    exists <- Directory.doesDirectoryExist $ UniPath.append "cabal-dev" Common.flowboxPath
    loggerIO debug "Checking for Flowbox configuration." 
    if exists
        then loggerIO debug "Configuration already exists."
        else loggerIO debug "Configuration does not exist."
    return exists


checkedInitialize :: IO ()
checkedInitialize = do
    initialized <- isAlreadyInitilized
    when (not initialized) initialize


initialize :: IO ()
initialize = do
    loggerIO info "Configuring Flowbox for the first use."
    Directory.createDirectoryIfMissing True $ Common.flowboxPath
    Process.runProcessInFolder Common.flowboxPath "cabal-dev" ["update"] 
