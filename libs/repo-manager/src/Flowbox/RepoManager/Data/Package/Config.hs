---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Flowbox.RepoManager.Data.Package.Config (
    --loadPackage
) where

import           Control.Applicative
import qualified Control.Exception   as Exception
import qualified Data.Configurator   as Configurator
import           Data.Text           as Text

import           Data.Configurator.Types                  (Value)
import qualified Data.Configurator.Types                  as Configurator
import qualified Data.Map                                 as Map
import           Flowbox.Prelude                          hiding (error)
import           Flowbox.RepoManager.Data.Dependency      as Dependency
import qualified Flowbox.RepoManager.Data.Environment     as Environment
import qualified Flowbox.RepoManager.Data.Package.Package as Package
import qualified Flowbox.RepoManager.Data.Version         as Version
import           Flowbox.System.Log.Logger
import qualified Network.URI                              as URI
import qualified Prelude
import qualified System.FilePath                          as FilePath
import qualified Text.ParserCombinators.ReadP             as ReadP

logger :: LoggerIO
logger = getLoggerIO $(moduleName)

fromString :: Value -> Text
fromString (Configurator.String a) = a

fromList :: Value -> [Value]
fromList (Configurator.List a) = a

--loadPackage :: FilePath -> IO Package.Package
--loadPackage file = do cfgFile <- Configurator.load [Configurator.Required file]

--                   let readConf fieldName = Exception.onException (fromJust =<< (Configurator.lookup cfgFile fieldName :: IO (Maybe Value)))
--                                     $ logger error ("Error reading config variable '" ++ show fieldName)

--                   Package.Package <$> (getPackageName     <$> readConf "name")
--                             <*> (getVersion      <$> readConf "version")
--                             <*> (getSources      <$> readConf "source")
--                             <*> (getScript       <$> readConf "install")
--                             <*> (getScript       <$> readConf "uninstall")
--                             <*> (getDependencies <$> readConf "dependencies")


getPackageName :: Value -> String
getPackageName nameString = Text.unpack $ fromString nameString

--getVersion :: Value -> Version.Version
--getVersion versionString = Prelude.head [x | (x,"") <- ReadP.readP_to_S Version.parseVersion $ Text.unpack $ fromString versionString]

--getSources :: Value -> Map.Map String URI.URI
--getSources sourcesValue = Map.fromList $ fmap (getArchitecture.fromList) (fromList sourcesValue)
--    where getArchitecture (architecture:uri:[]) = (Text.unpack $ fromString architecture, uriKind $ fromString uri)
--          uriKind uriString = furi $ Text.unpack uriString
--              where furi = if Text.isPrefixOf "http" uriString
--                              then Environment.Remote
--                              else Environment.Local

getScript :: Value -> [String]
getScript scriptValue = fmap (Text.unpack . fromString) (fromList scriptValue)

--getDependencies :: Value -> [Dependency]
--getDependencies dependenciesValue = fmap (getDependency . fromList) (fromList dependenciesValue)
--    where getDependency (package:deps) = Dependency.Dependency (Text.unpack $ fromString package) (constraint deps)
--          constraint deps = case deps of
--                                  []                 -> []
--                                  (_cons:[])         -> [Version.Exclude (Version.Range Nothing Nothing)]
--                                  (_cons1:_cons2:[]) -> [Version.Exclude (Version.Range Nothing Nothing)]


--loadConfig :: FilePath -> IO (Either String Package.Package)
--loadConfig file = do config <- Configurator.load [Configurator.Required file]

--                     let fileBaseName = FilePath.takeBaseName file
--                         name    = Prelude.takeWhile (/= '-') fileBaseName
--                         version = Prelude.tail . Prelude.dropWhile (/= '-') $ fileBaseName

--                     sources <- Configurator.require config "source"
--                     install <- Configurator.require config "install"
--                     uninstall <- Configurator.require config "uninstall"
--                     dependencies <- Configurator.require config "dependencies"

--                     return $ Right $ Package.Package name version sources install
