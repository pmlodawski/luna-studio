---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.RepoManager.Data.Item.Item where

import           Data.Map                             (Map)
import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Dependency  as Dependency
import           Flowbox.RepoManager.Data.Environment
import qualified Flowbox.RepoManager.Data.Version     as Version
import qualified Network.URI                          as URI
import qualified Data.List.Split                      as Split
import qualified Data.Maybe                           as Maybe
import qualified Data.Configurator                    as Configurator
import qualified Data.Configurator.Types              as Configurator
import qualified Data.Text                            as Text
import qualified Control.Exception                    as Exception
import qualified System.FilePath                      as FilePath
import           Data.Text.Lens                       (packed)

data Package = Package { _name         :: String
                       , _category     :: String
                       , _description  :: String
                       , _version      :: Version.Version
                       , _source       :: URI.URI
                       , _dependencies :: [Dependency.Dependency]
                       , _install      :: [Command]
                       , _uninstall    :: [Command]
                       } deriving (Show)

makeLenses ''Package

readBuildFile :: FilePath -> IO Package
readBuildFile file = do buildFile <- Configurator.load [Configurator.Required file]

                        let (name', version') = pathToNameVersion file

                        source'       <- readSource buildFile
                        category'     <- return "dummy"
                        description'  <- Configurator.require buildFile ("description" ^. packed) :: IO String
                        dependencies' <- readDependencies buildFile
                        install'      <- readScript buildFile ("install" ^. packed)
                        uninstall'    <- readScript buildFile ("uninstall" ^. packed)

                        return $ Package name' category' description' version' source' dependencies' install' uninstall'

readSource :: Configurator.Config -> IO URI.URI
readSource conf = do stringSource <- Configurator.require conf ("source" ^. packed) :: IO String
                     case URI.parseURI stringSource of
                         Just uri -> return uri
                         _        -> Exception.throwIO . Configurator.KeyError $ "source" ^. packed

readDependencies :: Configurator.Config -> IO [Dependency.Dependency]
readDependencies conf = do depends <- Configurator.require conf ("dependencies" ^. packed) :: IO Configurator.Value
                           case depends of
                               Configurator.List vals -> return $ map parseDependency $ Maybe.mapMaybe toString vals
                               _                      -> Exception.throwIO . Configurator.KeyError $ "dependencies" ^. packed

readScript :: Configurator.Config -> Text.Text -> IO [String]
readScript conf field = do commands <- Configurator.require conf field :: IO Configurator.Value
                           case commands of
                               Configurator.List vals -> return $ Maybe.mapMaybe toString vals
                               _                      -> Exception.throwIO . Configurator.KeyError $ field

pathToNameVersion :: FilePath -> (String, Version.Version)
pathToNameVersion = splitPackageName . FilePath.takeBaseName
    where splitPackageName pkgName = (takeWhile (/= '-') pkgName, Version.parseVersion $ tail $ dropWhile (/= '-') pkgName)

toString :: Configurator.Value -> Maybe String
toString (Configurator.String s) = Just $ s ^. from packed
toString _          = Nothing

parseDependency :: String -> Dependency.Dependency
parseDependency str = case splitted of
    [package]                     -> Dependency.Dependency package [Version.Any]
    [package, relation, version'] -> Dependency.Dependency package [(toRange relation $ Version.parseVersion version')]
    where splitted = Split.splitOn " " str
          toRange "<=" = Version.Maximum Version.NotStrict
          toRange "<"  = Version.Maximum Version.Strict
          toRange "==" = Version.Exactly
          toRange ">"  = Version.Minimum Version.Strict
          toRange ">=" = Version.Minimum Version.NotStrict