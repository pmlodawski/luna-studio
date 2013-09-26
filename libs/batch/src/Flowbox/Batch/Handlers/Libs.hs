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
    libraryRootDef,
) where

import qualified Data.Maybe                           as Maybe

import           Flowbox.Prelude                        
import qualified Flowbox.Batch.Batch                  as Batch
import           Flowbox.Batch.Batch                    (Batch)
import           Flowbox.Batch.Handlers.Common          (noresult, readonly, readonly', libManagerOp, libManagerOp', libraryOp, libraryOp', definitionOp)
import qualified Flowbox.Batch.Project.Project        as Project
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import qualified Flowbox.Data.String                  as String
import           Flowbox.Luna.Data.Source               (Source(Source))
import qualified Flowbox.Luna.Lib.LibManager          as LibManager
import qualified Flowbox.Luna.Lib.Library             as Library
import           Flowbox.Luna.Lib.Library               (Library)
import qualified Flowbox.Luna.Network.Def.Definition  as Definition
import           Flowbox.Luna.Network.Def.Definition    (Definition)
import qualified Flowbox.Lunac.Builder                as Builder
import qualified Flowbox.Luna.Tools.Serialize.Lib     as LibSerialization
import qualified Flowbox.Lunac.Diagnostics            as Diagnostics
import qualified Flowbox.System.UniPath               as UniPath
import           Flowbox.System.UniPath                 (UniPath)



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


buildLibrary :: Library.ID -> Project.ID -> Batch -> IO ()
buildLibrary libID projectID = readonly' . libraryOp' libID projectID (\batch library -> do
    let aprojectManager = Batch.projectManager batch
        proj = Maybe.fromJust $ ProjectManager.lab aprojectManager projectID
        
        diag        = Diagnostics.all
        projectName = Library.name library
        
        outputPath = UniPath.append ("build/hs/" ++ projectName) $ Project.path proj
        tmpName    = "tmp/" ++ projectName

        launcher = Source  ["Main"]
                 $ unlines [ "import " ++ (String.toUpper projectName) ++ " as M"
                           , "main = M.main"]

    Builder.initializeCabalDev

    sources <- Builder.buildLibrary diag library
    Builder.buildSources tmpName (launcher : sources)
    Builder.runCabal tmpName projectName
    Builder.moveExecutable tmpName projectName outputPath
    Builder.cleanUp tmpName 

    return (library, ()))


libraryRootDef :: Library.ID -> Project.ID -> Batch -> Either String (Definition.ID, Definition)
libraryRootDef libID projectID = readonly . definitionOp Library.rootDefID libID projectID (\_ definition ->  
    Right (definition, (Library.rootDefID, definition)))