---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handlers.Libs (
    libraries,

    libraryByID,
    createLibrary,
    loadLibrary,
    unloadLibrary,
    storeLibrary,
    buildLibrary,
    runLibrary,
    libraryRootDef,
) where

import qualified System.Process                      as Process

import           Flowbox.Prelude                       
import qualified Flowbox.Batch.Batch                 as Batch
import           Flowbox.Batch.Batch                   (Batch)
import           Flowbox.Batch.Handlers.Common         (noresult, readonly, readonly', libManagerOp, libManagerOp', libraryOp, libraryOp', definitionOp)
import qualified Flowbox.Batch.Project.Project       as Project
import qualified Flowbox.Luna.Lib.LibManager         as LibManager
import qualified Flowbox.Luna.Lib.Library            as Library
import           Flowbox.Luna.Lib.Library              (Library)
import qualified Flowbox.Luna.Network.Def.Definition as Definition
import           Flowbox.Luna.Network.Def.Definition   (Definition)
import qualified Flowbox.Lunac.Builder.Graph         as GraphBuilder
import qualified Flowbox.Luna.Tools.Serialize.Lib    as LibSerialization
import qualified Flowbox.Lunac.Diagnostics           as Diagnostics
import           Flowbox.System.Log.Logger             
import qualified Flowbox.System.Platform             as Platform
import qualified Flowbox.System.UniPath              as UniPath
import           Flowbox.System.UniPath                (UniPath)



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handlers.Libs"


libraries :: Project.ID -> Batch -> Either String [(Library.ID, Library)]
libraries projectID = readonly . libManagerOp projectID (\_ libManager -> 
    let r = LibManager.labNodes libManager 
    in Right (libManager, r))


libraryByID :: Library.ID -> Project.ID -> Batch -> Either String Library
libraryByID libID projectID = readonly . libraryOp libID projectID (\_ library -> do
    return (library, library))


createLibrary :: String -> UniPath -> Project.ID -> Batch -> Either String (Batch, (Library.ID, Library))
createLibrary name path projectID = libManagerOp projectID (\_ libManager -> do
    let library                = Library.make name path
        (newLibManager, libID) = LibManager.insNewNode library libManager
    return (newLibManager, (libID, library)))


loadLibrary :: UniPath -> Project.ID -> Batch -> IO (Batch, (Library.ID, Library))
loadLibrary path projectID = libManagerOp' projectID (\_ libManager -> do
    r <- LibManager.loadLibrary path libManager
    return r)


unloadLibrary :: Library.ID -> Project.ID -> Batch -> Either String Batch
unloadLibrary libID projectID = noresult . libManagerOp projectID (\_ libManager -> 
    let newLibManager = LibManager.delNode libID libManager
    in Right (newLibManager, ()))


storeLibrary :: Library.ID -> Project.ID -> Batch -> IO ()
storeLibrary libID projectID = readonly' . libraryOp' libID projectID (\_ library -> do
    LibSerialization.storeLibrary library
    return (library, ()))


-- TODO [PM] : More remote arguments needed
buildLibrary :: Library.ID -> Project.ID -> Batch -> IO ()
buildLibrary libID projectID = readonly' . libraryOp' libID projectID (\batch library -> do
    let config      = Batch.config batch
        diag        = Diagnostics.all
        projectName = Library.name library
        outputPath  = UniPath.fromUnixString projectName
        libs        = ["flowbox-stdlib"] -- TODO [PM] : hardoded libs and flags
        flags       = []
    GraphBuilder.build config diag library projectName "1.0" outputPath libs flags--TODO [PM] : hardcoded version
    return (library, ()))
    

-- TODO [PM] : Needs architecture change
runLibrary ::  Library.ID -> Project.ID -> Batch -> IO String
runLibrary libID projectID = readonly' . libraryOp' libID projectID (\_ library -> do
    let projectName = Library.name library
        command = Platform.dependent ("./" ++ projectName) (projectName ++ ".exe") ("./" ++ projectName)
        noStandardInput = ""
        noArguments     = [] --TODO [PM] : reimplement all this method to support real programs
    loggerIO debug $ "Running command '" ++ command ++ "'"
    (errorCode, stdOut, stdErr) <- Process.readProcessWithExitCode command noArguments noStandardInput
    let exitMsg = "Program exited with " ++ (show errorCode) ++ " code"
    loggerIO debug exitMsg
    return (library, stdOut ++ "\n" ++ "Program exited with " ++ (show errorCode) ++ " code"))


libraryRootDef :: Library.ID -> Project.ID -> Batch -> Either String (Definition.ID, Definition)
libraryRootDef libID projectID = readonly . definitionOp Library.rootDefID libID projectID (\_ definition ->  
    Right (definition, (Library.rootDefID, definition)))