---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Lib.Env where

import qualified Data.Maybe as Maybe

import           Flowbox.Prelude
import qualified Flowbox.System.Directory          as Directory
import           Flowbox.System.UniPath            (UniPath)
import qualified Flowbox.System.UniPath            as UniPath
import qualified Luna.Data.Serialize.Proto.Library as LibSerialization
import           Luna.Lib.Lib                      (Library)
import qualified Luna.Lib.Lib                      as Library
import qualified Luna.Lib.Path                     as Path



libraryExt :: String
libraryExt = ".lunalib"


mkLibraryFileName :: String -> String
mkLibraryFileName = (++ libraryExt)


putLibrary :: Library -> Bool -> IO ()
putLibrary library global = do
    installPath <- Path.installPath global
    let storePath = UniPath.append (mkLibraryFileName $ library ^. Library.name) installPath
    print storePath
    LibSerialization.storeLibrary library $ Just storePath


getLibrary :: String -> IO (Maybe Library)
getLibrary libraryName =
    getLibraryPath libraryName >>=
    Maybe.maybe (return Nothing) (fmap Just . LibSerialization.restoreLibrary)


getLibraryPath :: String -> IO (Maybe UniPath)
getLibraryPath libraryName =
    selectPath (mkLibraryFileName libraryName) =<< Path.searchPaths


selectPath :: String -> [UniPath] -> IO (Maybe UniPath)
selectPath _           []    = return Nothing
selectPath libFileName (h:t) = do
    let libFilePath = UniPath.append libFileName h
    exists <- Directory.doesFileExist libFilePath
    if exists then return (Just libFilePath) else selectPath libFileName t


