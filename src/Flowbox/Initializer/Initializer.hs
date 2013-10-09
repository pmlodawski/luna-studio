---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Initializer.Initializer where

import           Control.Applicative                  
import           Control.Monad                        
import qualified Data.String.Utils                  as StringUtils
import qualified System.IO                          as IO

import           Flowbox.Prelude                    hiding (error)
import qualified Flowbox.Config.Config              as Config
import           Flowbox.Config.Config                (Config)
import qualified Flowbox.System.Directory.Directory as Directory
import           Flowbox.System.Log.Logger            
import qualified Flowbox.System.Process             as Process
import qualified Flowbox.System.UniPath             as UniPath
import           Flowbox.System.UniPath               (UniPath)

-- TODO[PM] : Finish implementation


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Initializer.Initializer"


--successfullInstallFileName :: UniPath
--successfullInstallFileName = UniPath.append "installed" Common.flowboxPath


isAlreadyInitilized :: Config -> IO Bool
isAlreadyInitilized config = do 
    logger debug "Checking for Flowbox configuration."
    --exists_cabalDev  <- Directory.doesDirectoryExist $ UniPath.append "cabal-dev" Common.flowboxPath
    --exists_installed <- Directory.doesFileExist successfullInstallFileName
    --let exists = exists_cabalDev && exists_installed 
    let exists = False
    if exists
        then logger debug "Configuration already exists."
        else logger debug "Configuration does not exist or is broken."
    return exists 


initializeIfNeeded :: Config -> IO ()
initializeIfNeeded config = do
    initialized <- isAlreadyInitilized config
    when (not initialized) (clear config *> initialize config)


initialize :: Config -> IO ()
initialize config = do
    logger info "Configuring Flowbox for the first use. Please wait..."
    let global     = Config.global config
        local      = Config.local  config 
        localPkgDb = Config.pkgDb local
        localCabal = Config.cabal local
        ghcPkgBin  = Config.ghcPkg . Config.wrappers $ config
        cabalConfT = Config.cabal . Config.templates $ config
        cabalConf  = Config.cabal . Config.config $ config

    Directory.createDirectoryIfMissing True $ UniPath.fromUnixString localCabal
    Directory.createDirectoryIfMissing True $ UniPath.fromUnixString localPkgDb
    Process.runProcess Nothing ghcPkgBin ["recache", "--package-db=" ++ localPkgDb] 

    cabalConfTContent <- IO.readFile cabalConfT
    let cabalConfContent = StringUtils.replace "${FB_INSTALL}"    (Config.path global)
                         $ StringUtils.replace "${FB_HOME_CABAL}" (Config.cabal local) cabalConfTContent
    IO.writeFile cabalConf cabalConfContent
    
    --TODO[pm]: add cabal update

    ----Directory.touchFile successfullInstallFileName
    --logger info "Flowbox configured successfully."


clear :: Config -> IO ()
clear config = do 
    logger info "Cleaning Flowbox configuration."
    --exists <- Directory.doesDirectoryExist Common.flowboxPath
    --if exists
    --    then Directory.removeDirectoryRecursive Common.flowboxPath
    --    else return ()