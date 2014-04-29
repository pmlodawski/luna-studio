module Flowbox.RepoManager.Data.Hooks.Install where

import           Flowbox.Prelude
import           Control.Monad.Error                               (Error)
import qualified Control.Exception                                 as Exception
import qualified Flowbox.RepoManager.Data.Dependency               as Dependency
import qualified Flowbox.RepoManager.Data.Package.Package          as Package
import qualified Flowbox.RepoManager.Data.Package.ConfiguredPackage as ConfiguredPackage
import qualified Flowbox.RepoManager.Data.Package.Flag             as Flag
import qualified Flowbox.RepoManager.Data.Package.InstalledPackage as InstalledPackage
import qualified Flowbox.RepoManager.Data.RepoConfig               as RepoConfig
import qualified Flowbox.RepoManager.Data.Version                  as Version
import qualified System.FilePath                                   as FilePath
import qualified System.Directory                                  as Directory


installPackage :: RepoConfig.RepoConfig a -> ConfiguredPackage.ConfiguredPackage -> [Flag.Flag] -> IO InstalledPackage.InstalledPackage
installPackage config package flags = undefined
                                         

topoSortDependencies :: [Dependency.FixedDependency] -> [Dependency.FixedDependency]
topoSortDependencies deps = undefined
                                         

resolveDependencies :: RepoConfig.RepoConfig a -> Package.Package -> [Flag.Flag] -> IO (Either String [Package.Package])
resolveDependencies config package = undefined
