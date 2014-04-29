module Flowbox.RepoManager.Data.Hooks.Resolution where

import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Package.Family            as Family
import qualified Flowbox.RepoManager.Data.Package.Flag              as Flag
import qualified Flowbox.RepoManager.Data.Package.Package           as Package
import qualified Flowbox.RepoManager.Data.Package.ConfiguredPackage as ConfiguredPackage
import qualified Flowbox.RepoManager.Data.Dependency                as Dependency
import qualified Flowbox.RepoManager.Data.Version                   as Version
import qualified Flowbox.RepoManager.Data.RepoConfig                as RepoConfig
import qualified Flowbox.RepoManager.Data.Repository                as Repository
import qualified Flowbox.RepoManager.Utils.Utils                    as Utils
import qualified Flowbox.RepoManager.VCS.VCS                        as VCS
import qualified Data.List                                          as List
import qualified Data.Map                                           as Map
import qualified Data.Set                                           as Set

type DependencyGraph = [(Family.PackageFamily, [Family.PackageFamily])]

buildDependencyGraph :: VCS.VCS a => RepoConfig.RepoConfig a -> [Package.Package] -> IO DependencyGraph
buildDependencyGraph config packages = do let dependencies = concatMap Package._dependencies packages
                                              dependenciesNames = map Dependency.depName dependencies
                                          dependenciesFamilies <- 
                                              mapM (Repository.readPackageFamily $ RepoConfig.repositoryPath config) dependenciesNames
                                          undefined

dependencyToFamily :: RepoConfig.RepoConfig a -> Dependency.Dependency -> Family.PackageFamily
dependencyToFamily config dep = undefined

getDependenciesNames :: Package.Package -> Set.Set PackageFamily
getDependenciesNames pkg = Set.fromList $ map Dependency.depName $ Package._dependencies pkg

getDeps :: [Package.Package] -> Set.Set String
getDeps pkgs = getDeps1 pkgs Set.empty

getDeps1 :: [Package.Package] -> Set.Set String -> Set.Set String
getDeps1 []     acc = acc
getDeps1 (a:as) acc = getDeps1 (deps' ++ as) (acc `Set.union` deps')
    where deps' = getDependenciesNames a

-- call with empty graph will result in Left result
--removeOneItem :: DependencyGraph -> Either String (DependencyGraph, ConfiguredPackage.ConfiguredPackage)
--removeOneItem graph = case List.find packageWithoutDependencies graph of
--                          Just package -> Right (resultingGraph, fst package)
--                          Nothing      -> Left "circular dependency found"
--    where packageWithoutDependencies = null . snd
--          resultingGraph             = undefined

findSatisfyingPackage :: RepoConfig.RepoConfig a -> Dependency.Dependency -> IO Dependency.FixedDependency
findSatisfyingPackage config dependency = undefined
    -- list package versions
    -- get highest satisfying
    -- construct fixed dep

listPackageVersions :: RepoConfig.RepoConfig a -> Dependency.Dependency -> IO [Version.Version]
listPackageVersions config dep = --do filesInDir <- Utils.listPackageVersions
                                    undefined
