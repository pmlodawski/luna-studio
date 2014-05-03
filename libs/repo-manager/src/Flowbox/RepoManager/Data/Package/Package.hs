---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.RepoManager.Data.Package.Package where

import           Data.Map                             (Map)
import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Dependency  as Dependency
import           Flowbox.RepoManager.Data.Environment (Command)
import qualified Flowbox.RepoManager.Data.Package.Flag as Flag
import qualified Flowbox.RepoManager.Data.Types       as Types
import qualified Flowbox.RepoManager.Data.Version     as Version
import qualified Network.URI                          as URI
import qualified Data.List.Split                      as Split
import qualified Data.Maybe                           as Maybe
import qualified Data.Configurator                    as Configurator
import qualified Data.Configurator.Types              as Configurator
import qualified Data.List                            as List
import qualified Data.Text                            as Text
import           Data.Text.Lens                       (packed)
import qualified Distribution.Version                 as CabalVersion
import qualified Control.Exception                    as Exception
import qualified System.FilePath                      as FilePath

data Package = Package { _pkgName      :: Types.QualifiedPackageName
                       , _description  :: String
                       , _flags        :: [Flag.Flag]
                       , _defaultFlags :: [Flag.Flag]
                       , _version      :: Version.Version
                       , _source       :: URI.URI
                       , _dependencies :: [Dependency.Dependency]
                       , _install      :: [Command]
                       , _uninstall    :: [Command]
                       } deriving (Show)

makeLenses ''Package

instance Eq Package where
    p1 == p2 = (p1 ^. pkgName == p2 ^. pkgName) && (p1 ^. version == p2 ^. version)

instance Ord Package where
    compare p1 p2 = compare (p1 ^. pkgName) (p2 ^. pkgName) ++ compare (p1 ^. version) (p2 ^. version)


readBuildFile :: FilePath -> IO Package
readBuildFile file = do buildFile <- Configurator.load [Configurator.Required file]

                        let (name', version') = pathToNameVersion file

                        source'       <- readSource buildFile
                        category'     <- return ["dummy"]
                        let qualpkgname = Types.QualifiedPackageName name' category'
                        description'  <- Configurator.require buildFile "description" :: IO String
                        flags'        <- return []
                        defaultFlags' <- return []
                        dependencies' <- readDependencies buildFile
                        install'      <- readScript buildFile "install"
                        uninstall'    <- readScript buildFile "uninstall"

                        return $ Package qualpkgname description' flags' defaultFlags' version' source' dependencies' install' uninstall'

readSource :: Configurator.Config -> IO URI.URI
readSource conf = do stringSource <- Configurator.require conf "source" :: IO String
                     case URI.parseURI stringSource of
                         Just uri -> return uri
                         _        -> Exception.throwIO $ Configurator.KeyError "source"

-- FIXME[MM]: Intersect version ranges of same package
readDependencies :: Configurator.Config -> IO [Dependency.Dependency]
readDependencies conf = do depends <- Configurator.require conf "dependencies" :: IO Configurator.Value
                           case depends of
                               Configurator.List vals -> return $ map parseDependency $ Maybe.mapMaybe toString vals
                               _                      -> Exception.throwIO $ Configurator.KeyError "dependencies"
    where groupByName deps  = List.groupBy (\x y -> Dependency.qualDepName x == Dependency.qualDepName y)
          foldVersionRanges versionRanges = List.foldl1' CabalVersion.intersectVersionRanges versionRanges

          collapseSamePackageDependency deps = exampleDep { Dependency.constraints = unifiedVersionRange }
              where exampleDep = head deps
                    unifiedVersionRange = CabalVersion.simplifyVersionRange $ foldVersionRanges $ map Dependency.constraints deps

readScript :: Configurator.Config -> Text.Text -> IO [String]
readScript conf field = do commands <- Configurator.require conf field :: IO Configurator.Value
                           case commands of
                               Configurator.List vals -> return $ Maybe.mapMaybe toString vals
                               _                      -> Exception.throwIO $ Configurator.KeyError $ field

pathToNameVersion :: FilePath -> (String, Version.Version)
pathToNameVersion = splitPackageName . FilePath.takeBaseName
    where splitPackageName pkgName = (takeWhile (/= '-') pkgName, Version.parseVersion $ tail $ dropWhile (/= '-') pkgName)

toString :: Configurator.Value -> Maybe String
toString (Configurator.String s) = Just $ s ^. from packed
toString _          = Nothing

parseDependency :: String -> Dependency.Dependency
parseDependency str = case splitted of
        [package]                     -> Dependency.Dependency (toQualified package) CabalVersion.anyVersion []
        [package, relation, version'] -> Dependency.Dependency (toQualified package) (toRange relation $ Version.parseVersion version') []
    where splitted = Split.splitOn " " str
          toRange "<=" = CabalVersion.orEarlierVersion
          toRange "<"  = CabalVersion.earlierVersion
          toRange "==" = CabalVersion.thisVersion
          toRange ">"  = CabalVersion.laterVersion
          toRange ">=" = CabalVersion.orLaterVersion

          pkgName str = last $ Split.splitOn "/" str
          cat str     = init $ Split.splitOn "/" str

          toQualified str = Types.QualifiedPackageName (pkgName str) (cat str)
