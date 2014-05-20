---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.RepoManager.Data.Package.Package where

import           Control.Monad                        (join)
import           Data.Map                             (Map)
import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Dependency  as Dependency
import           Flowbox.RepoManager.Data.Environment (Command)
import qualified Flowbox.RepoManager.Data.Package.Flag as Flag
import qualified Flowbox.RepoManager.Data.Types       as Types
import qualified Flowbox.RepoManager.Data.Version     as Version
import qualified Flowbox.RepoManager.Utils.Utils      as Utils
import qualified Network.URI                          as URI
import qualified Control.Error.Util                   as Errors
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

-- FIXME[MM]: throws whatever error Configurator.load throws, should be made more safe
readBuildFile :: FilePath -> IO (Either Configurator.KeyError Package)
readBuildFile file = do buildFile <- Configurator.load [Configurator.Required file]

                        let (_, version') = pathToNameVersion file
                            qualPkgName   = Errors.note (Configurator.KeyError "name") $ Utils.packageFilePathToQualifiedName file

                        source'       <- readSource buildFile
                        description'  <- Right <$> Configurator.lookupDefault [] buildFile "description" :: IO (Either Configurator.KeyError String)
                        flags'        <- readFlags buildFile
                        defaultFlags' <- readScript buildFile "defaultFlags"
                        dependencies' <- readDependencies buildFile
                        install'      <- readScript buildFile "install"
                        uninstall'    <- readScript buildFile "uninstall"

                        return $ Package <$> qualPkgName
                                         <*> description'
                                         <*> flags'
                                         <*> (flags' >>= \k -> defaultFlags' >>= \l -> validateDefaultFlags l k)
                                         <*> pure version'
                                         <*> source'
                                         <*> dependencies'
                                         <*> install'
                                         <*> uninstall'

validateDefaultFlags :: [String] -> [Flag.Flag] -> Either Configurator.KeyError [Flag.Flag]
validateDefaultFlags defaultFlags allFlags = validateDefaultFlags' [] defaultFlags allFlags

validateDefaultFlags' :: [Flag.Flag] -> [String] -> [Flag.Flag] -> Either Configurator.KeyError [Flag.Flag]
validateDefaultFlags' acc []                 _        = Right acc
validateDefaultFlags' acc (defFlag:defFlags) allFlags = case List.find (\x -> Flag.name x == defFlag) allFlags of
                                                            Just flag -> validateDefaultFlags' (flag:acc) defFlags allFlags
                                                            _         -> Left $ Configurator.KeyError "defaultFlags"

readFlags :: Configurator.Config -> IO (Either Configurator.KeyError [Flag.Flag])
readFlags conf = Exception.try $ do flagsAsValue <- Configurator.require conf "flags" :: IO Configurator.Value
                                    case flagsAsValue of
                                        Configurator.List list -> do let flags = mapM parseFlag list
                                                                     case flags of
                                                                         Just f -> return f
                                                                         _      -> Exception.throwIO $ Configurator.KeyError "flags"
                                        _                      -> Exception.throwIO $ Configurator.KeyError "flags"

parseFlag :: Configurator.Value -> Maybe Flag.Flag
parseFlag (Configurator.List [Configurator.String name, Configurator.String desc]) = Just $ Flag.Flag (name ^. from packed) (desc ^. from packed)
parseFlag _                                                                        = Nothing

readSource :: Configurator.Config -> IO (Either Configurator.KeyError URI.URI)
readSource conf = Exception.try $ do stringSource <- Configurator.require conf "source" :: IO String
                                     case URI.parseURI stringSource of
                                         Just uri -> return uri
                                         _        -> Exception.throwIO $ Configurator.KeyError "source"

-- FIXME[MM]: Intersect version ranges of same package
readDependencies :: Configurator.Config -> IO (Either Configurator.KeyError [Dependency.Dependency])
readDependencies conf = Exception.try $ do depends <- Configurator.require conf "dependencies" :: IO Configurator.Value
                                           case depends of
                                               Configurator.List vals -> do let depString = mapM toString vals
                                                                                dependencies :: Maybe [Dependency.Dependency]
                                                                                dependencies = join $ mapM parseDependency <$> depString
                                                                            case dependencies of
                                                                                Just d -> return d
                                                                                _      -> Exception.throwIO $ Configurator.KeyError "dependencies" 
                                               _                      -> Exception.throwIO $ Configurator.KeyError "dependencies"
    where groupByName deps = List.groupBy (\x y -> Dependency.qualDepName x == Dependency.qualDepName y)
          foldVersionRanges versionRanges = List.foldl1' CabalVersion.intersectVersionRanges versionRanges

          collapseSamePackageDependency deps = exampleDep { Dependency.constraints = unifiedVersionRange }
              where exampleDep = head deps
                    unifiedVersionRange = CabalVersion.simplifyVersionRange $ foldVersionRanges $ map Dependency.constraints deps

readScript :: Configurator.Config -> Text.Text -> IO (Either Configurator.KeyError [String])
readScript conf field = Exception.try $ do commands <- Configurator.require conf field :: IO Configurator.Value
                                           case commands of
                                               Configurator.List vals -> do let script = mapM toString vals
                                                                            case script of
                                                                                Just s' -> return s'
                                                                                _       -> Exception.throwIO $ Configurator.KeyError $ field
                                               _                      -> Exception.throwIO $ Configurator.KeyError $ field

-- FIXME[MM]: does not support '-' in package name
pathToNameVersion :: FilePath -> (String, Version.Version)
pathToNameVersion = splitPackageName . FilePath.takeBaseName
    where splitPackageName pkgName = (takeWhile (/= '-') pkgName, Version.parseVersion $ tail $ dropWhile (/= '-') pkgName)

toString :: Configurator.Value -> Maybe String
toString (Configurator.String s) = Just $ s ^. from packed
toString _                       = Nothing

-- FIXME[MM]: parsing dependencies should use some parsing library (parsec?),
--            now it parses strings like "package == 1.0", while it should
--            allow specifying flags for each package
parseDependency :: String -> Maybe Dependency.Dependency
parseDependency str = case splitted of
        [package]                     -> 
            Dependency.Dependency <$> Types.makeQualified package <*> pure CabalVersion.anyVersion <*> pure []
        [package, relation, version'] ->
            Dependency.Dependency <$> Types.makeQualified package <*> pure (toRange relation $ Version.parseVersion version') <*> pure []
    where splitted = Split.splitOn " " str
          toRange "<=" = CabalVersion.orEarlierVersion
          toRange "<"  = CabalVersion.earlierVersion
          toRange "==" = CabalVersion.thisVersion
          toRange ">"  = CabalVersion.laterVersion
          toRange ">=" = CabalVersion.orLaterVersion
