module Flowbox.RepoManager.Data.Package.ConfiguredPackage where

import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Package.Flag as Flag
import qualified Flowbox.RepoManager.Data.Dependency   as Dependency
import           Flowbox.RepoManager.Data.Environment  (Command)
import qualified Flowbox.RepoManager.Data.Version      as Version
import qualified Network.URI                           as URI

data ConfiguredPackage = ConfiguredPackage { _name         :: String
                                           , _category     :: [String]
                                           , _description  :: String
                                           , _flags        :: [Flag.Flag]
                                           , _defaultFlags :: [Flag.Flag]
                                           , _setFlags     :: [Flag.Flag]
                                           , _version      :: Version.Version
                                           , _source       :: URI.URI
                                           , _dependencies :: [Dependency.FixedDependency]
                                           , _install      :: [Command]
                                           , _uninstall    :: [Command]
                                           } deriving Show