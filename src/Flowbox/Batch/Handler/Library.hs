---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.Library where

import           Data.Version   (Version (Version))
import qualified System.Process as Process

import           Flowbox.Batch.Batch               (Batch, gets)
import qualified Flowbox.Batch.Batch               as Batch
import           Flowbox.Batch.Handler.Common      (libManagerOp, libraryOp)
import qualified Flowbox.Batch.Handler.Common      as Batch
import qualified Flowbox.Batch.Project.Project     as Project
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Platform           as Platform
import           Flowbox.System.UniPath            (UniPath)
import qualified Flowbox.System.UniPath            as UniPath
import qualified Luna.Data.ASTInfo                 as ASTInfo
import qualified Luna.Data.Serialize.Proto.Library as LibSerialization
import           Luna.Lib.Lib                      (Library)
import qualified Luna.Lib.Lib                      as Library
import qualified Luna.Lib.Loader                   as LibLoader
import qualified Luna.Lib.Manager                  as LibManager
import qualified Luna.Pass.Build.Build             as Build
import           Luna.Pass.Build.BuildConfig       (BuildConfig (BuildConfig))
import qualified Luna.Pass.Build.BuildConfig       as BuildConfig
import qualified Luna.Pass.Build.Diagnostics       as Diagnostics


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.Library"


libraries :: Project.ID -> Batch [(Library.ID, Library)]
libraries projectID = LibManager.labNodes <$> Batch.getLibManager projectID


libraryByID :: Library.ID -> Project.ID -> Batch Library
libraryByID = Batch.getLibrary


createLibrary :: String -> UniPath -> Project.ID -> Batch (Library.ID, Library)
createLibrary name path projectID = libManagerOp projectID $ \libManager -> do
    let library                = Library.make name path [name]
        (newLibManager, libraryID) = LibManager.insNewNode library libManager
    return (newLibManager, (libraryID, library))


modifyLibrary :: (Library.ID, Library) -> Project.ID -> Batch ()
modifyLibrary (libraryID, library) projectID = libraryOp libraryID projectID $ \oldLibrary -> do
    let ast         = oldLibrary ^. Library.ast
        propertyMap = oldLibrary ^. Library.propertyMap
        newLibrary  = library & Library.ast .~ ast
                              & Library.propertyMap .~ propertyMap
    return (newLibrary, ())


loadLibrary :: UniPath -> Project.ID -> Batch (Library.ID, Library)
loadLibrary path projectID = libManagerOp projectID
    (liftIO . LibLoader.loadLibrary path)


unloadLibrary :: Library.ID -> Project.ID -> Batch ()
unloadLibrary libraryID projectID = libManagerOp projectID (\libManager ->
    return (LibManager.delNode libraryID libManager, ()))


storeLibrary :: Library.ID -> Project.ID -> Batch ()
storeLibrary libraryID projectID = do
    library <- Batch.getLibrary libraryID projectID
    liftIO $ LibSerialization.storeLibrary library


-- TODO [PM] : More remote arguments needed
buildLibrary :: Library.ID -> Project.ID -> Batch ()
buildLibrary libraryID projectID = do
    cfg         <- gets (view Batch.config)
    projectPath <- view Project.path <$> Batch.getProject projectID
    library     <- Batch.getLibrary projectID libraryID
    let ast         = library ^. Library.ast
        name        = library ^. Library.name
        version     = Version [1][]      -- TODO [PM] : hardcoded version
        diag        = Diagnostics.all   -- TODO [PM] : hardcoded diagnostics
        outputPath  = Platform.addExeOnWindows $ UniPath.append name projectPath
        libs        = []                 -- TODO [PM] : hardcoded libs
        ghcFlags    = ["-O2", "-threaded", "-Odph", "-optlo-O3"] -- TODO [PM] : hardcoded ghc flags
        cppFlags    = []                 -- TODO [PM] : hardcoded cpp flags
        cabalFlags  = []                 -- TODO [PM] : hardcoded cabal flags
        buildDir    = Nothing

        buildType   = BuildConfig.Executable outputPath -- TODO [PM] : hardoded executable type
        bldCfg      = BuildConfig name version libs ghcFlags cppFlags cabalFlags buildType cfg diag buildDir

    maxID <- Batch.getMaxID projectID libraryID
    EitherT $ Build.run bldCfg ast (ASTInfo.mk maxID) False


-- TODO [PM] : Needs architecture change
runLibrary ::  Library.ID -> Project.ID -> Batch ()
runLibrary libraryID projectID = do
    project <- Batch.getProject projectID
    let projectPath = project ^. Project.path
        libs        = project ^. Project.libs
    library <- LibManager.lab libs libraryID <??> "Wrong libraryID=" ++ show libraryID
    name    <- UniPath.toUnixString <$> UniPath.expand (UniPath.append (library ^. Library.name) projectPath)
    let command = Platform.dependent name (name ++ ".exe") name
    --    noStandardInput = ""
    --    noArguments     = [] --TODO [PM] : reimplement all this method to support real programs
    --loggerIO debug $ "Running command '" ++ command ++ "'"
    --(errorCode, stdOut, stdErr) <- Process.readProcessWithExitCode (Just projectPath) command noArguments noStandardInput
    --let exitMsg = "Program exited with " ++ (show errorCode) ++ " code"
    --loggerIO debug exitMsg
    --return (library, stdOut ++ stdErr ++ "\n" ++ "Program exited with " ++ (show errorCode) ++ " code"))
    void $ liftIO $ Process.runCommand command


interpretLibrary :: Library.ID -> Project.ID -> Batch ()
interpretLibrary = Batch.interpretLibrary
