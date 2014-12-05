---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Lib.Env where

import qualified Data.List       as List
import qualified Data.Maybe      as Maybe
import qualified System.FilePath as FilePath

import           Flowbox.Data.Version              (Version)
import qualified Flowbox.Data.Version              as Version
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


mkLibraryFileName :: String -> Version -> String
mkLibraryFileName name version = show (Version.Versioned (Version.Name name) version)
                              ++ libraryExt


putLibrary :: Library -> Path.Location -> IO ()
putLibrary library location = do
    installPath <- Path.installPath location
    let libFileName = mkLibraryFileName (library ^. Library.name) (library ^. Library.version)
        storePath   = UniPath.append libFileName installPath
    LibSerialization.storeLibrary library $ Just storePath


getLibrary :: String -> Version -> IO (Maybe Library)
getLibrary libraryName version =
    getLibraryPath libraryName version >>=
    Maybe.maybe (return Nothing) (fmap Just . LibSerialization.restoreLibrary)


getLibraryVersions :: String -> IO [Version]
getLibraryVersions libraryName =
    getVersions libraryName =<< Path.searchPaths


getNewsetLibrary :: String -> IO (Maybe Library)
getNewsetLibrary libraryName = do
    versions <- getLibraryVersions libraryName
    case versions of
        [] -> return Nothing
        _  -> getLibrary libraryName $ maximum versions


getLibraryPath :: String -> Version -> IO (Maybe UniPath)
getLibraryPath libraryName version =
    selectPath libraryName version =<< Path.searchPaths


selectPath :: String -> Version -> [UniPath] -> IO (Maybe UniPath)
selectPath _           _       []    = return Nothing
selectPath libraryName version (h:t) = do
    let libFileName = mkLibraryFileName libraryName version
        libFilePath = UniPath.append libFileName h
    exists <- Directory.doesFileExist libFilePath
    if exists then return (Just libFilePath) else selectPath libraryName version t


getVersions :: String -> [UniPath] -> IO [Version]
getVersions _           []    = return []
getVersions libraryName (h:t) = do
    dirExists <- Directory.doesDirectoryExist h
    found     <- if dirExists
        then do
            contents <- Directory.getDirectoryContents h
            return $ Maybe.catMaybes
                    $ map (Version.readVersionMaybe . fst)
                    $ filter ((==) libraryExt . snd)
                    $ map FilePath.splitExtension
                    $ Maybe.catMaybes
                    $ map (List.stripPrefix $ libraryName ++ "-") contents
        else return []
    ((++) found) <$> getVersions libraryName t
