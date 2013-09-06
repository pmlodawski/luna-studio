---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handlers.Libs (
    libraries,
    createLibrary,
    loadLibrary,
    unloadLibrary,
    storeLibrary,
    buildLibrary,
    libraryRootDef,

    libManagerOp,
    libManagerOp',
    libraryOp,
    libraryOp',
) where

import           Flowbox.Batch.Batch                   (Batch(..))
import           Flowbox.Batch.Handlers.Common         (noresult, readonly, readonly', activeProject, libManagerOp, libManagerOp', libraryOp, libraryOp', definitionOp)
import qualified Flowbox.Batch.Project.Project       as Project
import qualified Flowbox.Luna.Builder.Builder        as Builder
import           Flowbox.Luna.Builder.Builder          (Builder(..))
import qualified Flowbox.Luna.Lib.LibManager         as LibManager
import qualified Flowbox.Luna.Lib.Library            as Library
import           Flowbox.Luna.Lib.Library              (Library(..))
import qualified Flowbox.Luna.Network.Def.Definition as Definition
import           Flowbox.Luna.Network.Def.Definition   (Definition(..))
import qualified Flowbox.Luna.Tools.Serialize.Lib    as LibSerialization
import           Flowbox.System.UniPath                (UniPath)
import           Flowbox.System.Log.Logger                  


logger = getLoggerIO "Flowbox.Batch"


libraries :: Batch -> Either String [(Library.ID, Library)]
libraries = readonly . libManagerOp (\_ libManager -> 
    let r = LibManager.labNodes libManager 
    in Right (libManager, r))


createLibrary :: String -> UniPath -> Batch -> Either String (Batch, (Library.ID, Library))
createLibrary libName libPath = libManagerOp (\_ libManager -> 
    let library                = Library.make libName libPath
        (newLibManager, libID) = LibManager.insNewNode library libManager
    in Right (newLibManager, (libID, library)))


loadLibrary :: UniPath -> Batch -> IO (Batch, (Library.ID, Library))
loadLibrary lpath = libManagerOp' (\_ libManager -> do
    r <- LibManager.loadLibrary lpath libManager
    return r)


unloadLibrary :: Library.ID -> Batch -> Either String Batch
unloadLibrary libID = noresult . libManagerOp (\_ libManager -> 
    let newLibManager = LibManager.delNode libID libManager
    in Right (newLibManager, ()))


storeLibrary :: Library.ID -> Batch -> IO ()
storeLibrary libID = readonly' . libraryOp' libID (\_ library -> do
    LibSerialization.storeLibrary library
    return (library, ()))


buildLibrary :: Library.ID -> Batch -> IO ()
buildLibrary libID = readonly' . libraryOp' libID (\batch library -> do
    logger.warning $ "Build library not implemented"
    return (library, ()))


libraryRootDef :: Library.ID -> Batch -> Either String (Definition.ID, Definition)
libraryRootDef libID = readonly . definitionOp Library.rootDefID libID (\_ definition ->  
    Right (definition, (Library.rootDefID, definition)))