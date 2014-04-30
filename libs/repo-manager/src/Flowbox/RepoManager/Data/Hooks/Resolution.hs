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
import           Control.Monad                                      (guard)
import           Data.Function                                      (on)                                     
import qualified Data.List                                          as List
import qualified Data.Map                                           as Map
import qualified Data.Set                                           as Set
import qualified Distribution.Version                               as CabalVersion

type DependencyGraph = [(Family.PackageFamily, [Family.PackageFamily])]

buildDependencyGraph :: VCS.VCS a => RepoConfig.RepoConfig a -> [Package.Package] -> IO DependencyGraph
buildDependencyGraph config packages = do let dependencies = concatMap Package._dependencies packages
                                              dependenciesNames = map Dependency.depName dependencies
                                          dependenciesFamilies <- 
                                              mapM (Repository.readPackageFamily $ RepoConfig.repositoryPath config) dependenciesNames
                                          undefined

dependencyToFamily :: RepoConfig.RepoConfig a -> Dependency.Dependency -> Family.PackageFamily
dependencyToFamily config dep = undefined

--getDependenciesNames :: Package.Package -> Set.Set Family.PackageFamily
--getDependenciesNames pkg = Set.fromList $ map Dependency.depName $ Package._dependencies pkg

--getDeps :: [Package.Package] -> Set.Set String
--getDeps pkgs = getDeps1 pkgs Set.empty

--getDeps1 :: [Package.Package] -> Set.Set String -> Set.Set String
--getDeps1 []     acc = acc
--getDeps1 (a:as) acc = getDeps1 (deps' ++ as) (acc `Set.union` deps')
--    where deps' = getDependenciesNames a

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


--FIXME[mm]: szkic brute-force'a do rozwiazania zaleznosci
--type Dep = (String, [Int])
--type Pkg = (String, [Dep], Int)

--a1,a2,x1,x2,x3,x4,y1,y2,y3,w1,w2,z1 :: Pkg

--a1 = ("a", [("x", [1,2])], 1)
--a2 = ("a", [("x", [2,3,4])], 2)
--x1 = ("x", [], 1)
--x2 = ("x", [("y", [1])], 2)
--x3 = ("x", [("y", [2])], 3)
--x4 = ("x", [("y", [2,3])], 4)
--y1 = ("y", [], 1)
--y2 = ("y", [], 2)
--y3 = ("y", [], 3)
--w1 = ("w", [("y", [1])], 1)
--w2 = ("w", [("y", [2,3])], 2)
--z1 = ("z", [("w", [1])], 1)

--test :: [Pkg]
--test = [a1,a2,x1,x2,x3,x4,y1,y2,y3,w1,w2,z1]

--testpkg :: [[Pkg]]
--testpkg = [[a1,a2], [x1,x2,x3,x4], [y1,y2,y3], [w1,w2], [z1]]

--allPossibilities :: [[Pkg]] -> [[Pkg]]
--allPossibilities []         = return []
--allPossibilities (pkg:pkgs) = do pkg' <- pkg
--                                 map (pkg':) $ allPossibilities pkgs

--possibleSolutions :: [[Pkg]] -> [[Pkg]]
--possibleSolutions p = filter consistent p

--consistent :: [Pkg] -> Bool
--consistent pkgs = and $ map (depsSatisfied pkgs) pkgs
--    where depsSatisfied :: [Pkg] -> Pkg -> Bool
--          depsSatisfied pkgs' package = and $ map (depSatisfied otherPkgs) $ deps
--              where deps = package ^. _2
--                    otherPkgs = List.delete package pkgs'

--          depSatisfied :: [Pkg] -> Dep -> Bool
--          depSatisfied pkgs' dep = or $ map (=== dep) pkgs'

--          (===) :: Pkg -> Dep -> Bool
--          pkg === dep = pkg ^. _1 == dep ^. _1 && (pkg ^. _3) `elem` (dep ^. _2)


allPossibilities :: [[Package.Package]] -> [[Package.Package]]
allPossibilities []         = return []
allPossibilities (pkg:pkgs) = do pkg' <- pkg
                                 map (pkg':) $ allPossibilities pkgs

recursivelyAddDeps :: FilePath -> [Package.Package] -> IO [Package.Package]
recursivelyAddDeps config a = go config a Set.empty
    where go :: FilePath -> [Package.Package] -> Set.Set Package.Package -> IO [Package.Package]
          go config []     acc = return $ Set.toList acc
          go config (a:as) acc = do let readPackageFromPath = Repository.readPackage config
                                    aDependencies <- concat <$> mapM readPackageFromPath (map Dependency.dependencyQualifiedName $ a ^. Package.dependencies)
                                    go config (aDependencies ++ as) (acc `Set.union` Set.fromList (a : aDependencies))

groupByName :: [Package.Package] -> [[Package.Package]]
groupByName = List.groupBy (\x y -> Package._name x == Package._name y)
            . List.sortBy (compare `on` Package._version)
            . List.sortBy (compare `on` Package._name)

newestFirst :: [[Package.Package]] -> [[Package.Package]]
newestFirst = map reverse

consistent :: [Package.Package] -> Bool
consistent pkgs = and $ map (depsSatisfied pkgs) pkgs
    where depsSatisfied :: [Package.Package] -> Package.Package -> Bool
          depsSatisfied pkgs' package = and $ map (depSatisfied otherPkgs) $ deps
              where deps = package ^. Package.dependencies 
                    otherPkgs = List.delete package pkgs'

          depSatisfied :: [Package.Package] -> Dependency.Dependency -> Bool
          depSatisfied pkgs' dep = or $ map (=== dep) pkgs'

          (===) :: Package.Package -> Dependency.Dependency -> Bool
          p1 === p2 = p1 ^. Package.name == Dependency.depName p2 && p1 ^. Package.category == Dependency.depCategory p2 &&
                      CabalVersion.withinRange (p1 ^. Package.version) (Dependency.constraints p2)

possibleSolutions :: [[Package.Package]] -> [[Package.Package]]
possibleSolutions = filter consistent

resolveDependencies :: FilePath -> [Package.Package] -> IO (Maybe [Package.Package])
resolveDependencies repoPath explicitPackages = do graphToResolve <- recursivelyAddDeps repoPath explicitPackages
                                                   let groupedFamilies        = groupByName graphToResolve
                                                       searchSpace            = allPossibilities groupedFamilies
                                                       sortedFromNewest       = newestFirst searchSpace
                                                   return $ case possibleSolutions sortedFromNewest of
                                                       []           -> Nothing
                                                       (solution:_) -> Just solution
