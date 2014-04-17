{-# LANGUAGE TemplateHaskell #-}
module Flowbox.RepoManager.Testing where

import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Dependency as Dependency
import qualified System.Directory as Directory
import qualified Data.Version as Version
import qualified Network.URI  as URI
import qualified System.FilePath as FilePath
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Set as Set
import qualified Flowbox.System.Log.Logger as Logger
import qualified Data.Maybe as Maybe
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import           Data.Text.Lens (packed)

data Architecture = X86 | X64 | Any
    deriving (Read, Show)

data Source = Source { _uri  :: URI.URI
                     , _arch :: Architecture 
                     } deriving Show

type Command = String

data Package = Package { _name         :: String
                       , _version      :: Version.Version
                       , _source       :: [Source]
                       , _dependencies :: [Dependency.Dependency]
                       , _install      :: [Command]
                       , _uninstall    :: [Command]
                       } deriving Show

makeLenses ''Source
makeLenses ''Package

installPackage :: String -> IO ()
installPackage pkgName = undefined --do ebuild <- findBuildFile pkgName -- just name or name-version
                            --item <- parseConfig ebuild
                            --deps <- resolveDependencies
                            --mapM_ installPackage deps
                            --install item

downloadSources :: Architecture -> Package -> IO ()
downloadSources arch item = undefined

pathToNameVersion :: FilePath -> (String, Version.Version)
pathToNameVersion = splitPackageName . FilePath.takeBaseName
    where splitPackageName pkgName = (takeWhile (/= '-') pkgName, parseVersion $ tail $ dropWhile (/= '-') pkgName)
          parseVersion = fst . last . ReadP.readP_to_S Version.parseVersion

createDB :: FilePath -> Set.Set Package
createDB path = undefined

findConfigFiles :: FilePath -> IO [FilePath]
findConfigFiles dir = do files <- Directory.getDirectoryContents dir
                         let scripts = filter (\x -> FilePath.takeExtension x == ".config") files
                         return scripts

readBuildFile :: FilePath -> IO Package
readBuildFile file = do buildFile <- Configurator.load [Configurator.Required file]

                        let (packageName, packageVersion) = pathToNameVersion file

                        source <- readSource buildFile

                        undefined

readSource :: Configurator.Config -> IO [Source]
readSource conf = do stringSource <- Configurator.require conf ("source" ^. packed) :: IO [[String]]
                     let foo = map listToSource stringSource
                     return $ Maybe.catMaybes foo

listToSource :: [String] -> Maybe Source
listToSource [uri]       | URI.isURI uri = Just $ Source (Maybe.fromJust $ URI.parseURI uri) Any
listToSource [arch, uri] | URI.isURI uri = Just $ Source (Maybe.fromJust $ URI.parseURI uri) (read arch)
listToSource _                           = Nothing