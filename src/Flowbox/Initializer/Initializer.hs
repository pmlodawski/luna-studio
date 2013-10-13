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



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Initializer.Initializer"


successfullInstallFilePath :: Config -> UniPath
successfullInstallFilePath config = UniPath.append "installed" localPath where
    localPath = UniPath.fromUnixString $ Config.path $ Config.local config 


isAlreadyInitilized :: Config -> IO Bool
isAlreadyInitilized config = do 
    let local      = Config.local  config 
        localCabal = Config.cabal local
        localPkgDb = Config.pkgDb local
    logger debug "Checking for Flowbox configuration."
    exists_localCabal <- Directory.doesDirectoryExist $ UniPath.fromUnixString localCabal
    exists_localPkgDb <- Directory.doesDirectoryExist $ UniPath.fromUnixString localPkgDb
    exists_installed  <- Directory.doesFileExist $ successfullInstallFilePath config
    let exists = exists_localCabal && exists_localPkgDb && exists_installed
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
        localCabal = Config.cabal local
        localPkgDb = Config.pkgDb local
        ghcPkgBin  = Config.ghcPkg . Config.wrappers  $ config
        cabalConfT = Config.cabal  . Config.templates $ config
        cabalConf  = Config.cabal  . Config.config    $ config
        cabalBin   = Config.cabal  . Config.wrappers  $ config

    Directory.createDirectoryIfMissing True $ UniPath.fromUnixString localCabal
    Directory.createDirectoryIfMissing True $ UniPath.fromUnixString localPkgDb
    Process.runProcess Nothing ghcPkgBin ["recache", "--package-db=" ++ localPkgDb] 

    cabalConfTContent <- IO.readFile cabalConfT
    let cabalConfContent = StringUtils.replace "${FB_INSTALL}"    (Config.path global)
                         $ StringUtils.replace "${FB_HOME_CABAL}" (Config.cabal local) cabalConfTContent
    IO.writeFile cabalConf cabalConfContent
    Process.runProcess Nothing cabalBin ["update"] 
    Directory.touchFile $ successfullInstallFilePath config
    logger info "Flowbox configured successfully."


clear :: Config -> IO ()
clear config = do 
    logger info "Cleaning Flowbox configuration."
    let localPath = UniPath.fromUnixString $ Config.path $ Config.local config 
    exists <- Directory.doesDirectoryExist localPath
    if exists
        then Directory.removeDirectoryRecursive localPath
        else return ()