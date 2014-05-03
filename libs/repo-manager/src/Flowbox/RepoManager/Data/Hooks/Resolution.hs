module Flowbox.RepoManager.Data.Hooks.Resolution where

import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Package.Family  as Family
import qualified Flowbox.RepoManager.Data.Package.Package as Package
import qualified Flowbox.RepoManager.Data.Dependency      as Dependency
import qualified Flowbox.RepoManager.Data.Repository      as Repository
import qualified Flowbox.RepoManager.Data.Types           as Types
import qualified Flowbox.RepoManager.Data.Version         as Version
import qualified Flowbox.RepoManager.VCS.VCS              as VCS
import           Data.Function                            (on)                                     
import qualified Data.List                                as List
import qualified Data.Map                                 as Map
import qualified Data.Maybe                               as Maybe
import qualified Data.Set                                 as Set
import qualified Distribution.Version                     as CabalVersion

allPossibilities :: [[Package.Package]] -> [[Package.Package]]
allPossibilities []         = return []
allPossibilities (pkg:pkgs) = do pkg' <- pkg
                                 map (pkg':) $ allPossibilities pkgs

recursivelyAddDeps :: VCS.VCS a => Repository.Repository a -> [Package.Package] -> [Package.Package]
recursivelyAddDeps repo pkgs = go (Repository.packages repo) pkgs Set.empty
    where go :: Map.Map Types.QualifiedPackageName Family.PackageFamily -> [Package.Package] -> Set.Set Package.Package -> [Package.Package]
          go _  []     acc = Set.toList acc
          go db (a:as) acc = let aDeps' = map Dependency.qualDepName $ a ^. Package.dependencies
                                 aDepsFamilies :: [Family.PackageFamily]
                                 aDepsFamilies = Maybe.mapMaybe (flip Map.lookup db) aDeps'
                                 aDepsMaps :: [Map.Map Version.Version Package.Package] 
                                 aDepsMaps = map Family.versions aDepsFamilies
                                 aDepsPackages :: [Package.Package]
                                 aDepsPackages = concat $ map Map.elems aDepsMaps
                             in
                                 go db (aDepsPackages ++ as) (acc `Set.union` Set.fromList (a : aDepsPackages))
                                --aDependencies <- concat <$> mapM readPackageFromPath (map (Dependency.qualDepName) $ a ^. Package.dependencies)
                                --go config (aDependencies ++ as) (acc `Set.union` Set.fromList (a : aDependencies))

groupByName :: [Package.Package] -> [[Package.Package]]
groupByName = List.groupBy (\x y -> Package._pkgName x == Package._pkgName y)
            . List.sortBy (compare `on` Package._version)
            . List.sortBy (compare `on` Package._pkgName)

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
          p1 === p2 = p1 ^. Package.pkgName == Dependency.qualDepName p2 &&
                      CabalVersion.withinRange (p1 ^. Package.version) (Dependency.constraints p2)

possibleSolutions :: [[Package.Package]] -> [[Package.Package]]
possibleSolutions = filter consistent

resolveDependencies :: VCS.VCS a => Repository.Repository a -> [Package.Package] -> Maybe [Package.Package]
resolveDependencies repo explicitPackages = let graphToResolve   = recursivelyAddDeps repo explicitPackages
                                                groupedFamilies  = groupByName graphToResolve
                                                searchSpace      = allPossibilities groupedFamilies
                                                sortedFromNewest = newestFirst searchSpace
                                            in  case possibleSolutions sortedFromNewest of
                                                   []           -> Nothing
                                                   (solution:_) -> Just solution
