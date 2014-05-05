module Flowbox.RepoManager.Data.Hooks.Install where

import           Flowbox.Prelude
import qualified Control.Exception                                 as Exception
import           Control.Monad                                     (when)
import           Flowbox.RepoManager.Data.Environment              (Command)
import qualified Flowbox.RepoManager.Data.Hooks.Resolution         as Resolution
import qualified Flowbox.RepoManager.Data.Package.Package          as Package
import qualified Flowbox.RepoManager.Data.Package.Flag             as Flag
import qualified Flowbox.RepoManager.Data.Package.InstalledPackage as InstalledPackage
import qualified Flowbox.RepoManager.Data.RepoConfig               as RepoConfig
import qualified Flowbox.RepoManager.Data.Repository               as Repository
import qualified Flowbox.RepoManager.Data.Version                  as Version
import qualified Flowbox.RepoManager.Utils.Utils                   as Utils
import qualified Data.Digest.Pure.SHA                              as SHA
import qualified Data.Graph                                        as Graph
import qualified Data.List                                         as List
import qualified Data.Map                                          as Map
import qualified Data.Maybe                                        as Maybe
import qualified System.Directory                                  as Directory
import qualified System.Exit                                       as Exit
import qualified System.FilePath                                   as FilePath
import qualified System.Process                                    as Process
-- FIXME[MM]: cross-platform functions for manipulating environment are
--            available only in GHC 7.8 (base 4.7.0.0), so instead I used
--            the unix package
import qualified System.Posix.Env                                  as Env

type InstalledDependencies = [InstalledPackage.InstalledPackage]

-- assumes package source is already downloaded in RepoConfig.downloadPath
installPackage :: Repository.Repository a
                  -> Package.Package
                  -> [Flag.Flag]
                  -> InstalledDependencies
                  -> IO (Either String InstalledPackage.InstalledPackage)
installPackage repo package flags deps = let hash           = InstalledPackage.hashPackage package flags deps
                                             nameAndVersion = show (package ^. Package.pkgName)
                                                            ++ "-"
                                                            ++ Version.showVersion (package ^. Package.version)
                                             thisPackageDir = nameAndVersion ++ "[" ++ SHA.showDigest hash ++ "]"
                                             resultingPackage = InstalledPackage.makeInstalled package thisPackageDir flags deps hash
                                         in Utils.withDirectory (Repository.config repo ^. RepoConfig.downloadPath) $
                                                withEnv (prepareEnv repo thisPackageDir flags) $ do
                                                    dirExists <- Directory.doesDirectoryExist thisPackageDir
                                                    if dirExists then
                                                            return $ Left "Directory already exists. Probably this package was installed before" 
                                                        else do
                                                            Directory.createDirectoryIfMissing True thisPackageDir
                                                            installExitStatus <- tryExitCode $ runCommands $ package ^. Package.install
                                                            return $ case installExitStatus of
                                                                Left exitCode -> Left $ "Installation script exited with " ++ show exitCode
                                                                _             -> Right resultingPackage

type PackageFlagsMapping = [(Package.Package, [Flag.Flag])]

installDependencyGraph :: Repository.Repository a
                          -> Resolution.DependencyGraph
                          -> PackageFlagsMapping
                          -> IO (Either String [InstalledPackage.InstalledPackage])
installDependencyGraph repo depGraph explicitPkgs = installDependencyGraph' repo depGraph explicitPkgs topoSortedPackages Map.empty
    where topoSortedPackages = Resolution.topoSortDependencies' depGraph

installDependencyGraph' :: Repository.Repository a
                           -> Resolution.DependencyGraph
                           -> PackageFlagsMapping
                           -> [Package.Package]
                           -> Map.Map Graph.Vertex InstalledPackage.InstalledPackage
                           -> IO (Either String [InstalledPackage.InstalledPackage])
installDependencyGraph' _    _        _             []         alreadyInstalled = return $ Right $ Map.elems alreadyInstalled
installDependencyGraph' repo depGraph package2flags (pkg:pkgs) alreadyInstalled =
    let pkgDependenciesAsVertices = Resolution.getPackageDependencies depGraph pkg
        pkgInstalledDependencies  = sequence $ map (flip Map.lookup alreadyInstalled) pkgDependenciesAsVertices
        allDependenciesInstalled  = Maybe.isJust $ pkgInstalledDependencies
        pkgFlags                  = Maybe.fromMaybe [] $ List.lookup pkg package2flags
    in  if allDependenciesInstalled then do
            installResult <- installPackage repo pkg pkgFlags (Maybe.fromJust pkgInstalledDependencies)
            case installResult of
                Left s            -> return $ Left s
                Right installedid -> do let installedPackageVertex = Resolution.getPackageVertex depGraph pkg
                                            newInstalledMap        = Map.insert installedPackageVertex installedid alreadyInstalled
                                        installDependencyGraph' repo depGraph package2flags pkgs newInstalledMap
        else do
            return $ Left "Internal error: package dependencies should be installed but aren't. Probably toposort is broken."

tryExitCode :: IO a -> IO (Either Exit.ExitCode a)
tryExitCode = Exception.try

runCommands :: [Command] -> IO ()
runCommands []     = return ()
runCommands (c:cs) = do returnCode <- Process.system c
                        when (isFailure returnCode) $ Exception.throwIO returnCode
                        runCommands cs

isFailure :: Exit.ExitCode -> Bool
isFailure (Exit.ExitFailure _) = True
isFailure _                    = False

prepareEnv :: Repository.Repository a -> FilePath -> [Flag.Flag] -> [(String, String)]
prepareEnv repo packageDir flags = paths ++ map (\f -> (Flag.name f, "1")) flags
    where paths = [("DOWNLOAD_PATH", repoConfig ^. RepoConfig.downloadPath),
                   ("MAKE_PATH",     repoConfig ^. RepoConfig.makePath),
                   ("INSTALL_PATH",  repoConfig ^. RepoConfig.installPath FilePath.</> packageDir)]
          repoConfig = Repository.config repo

unexportEnvs :: [(String, String)] -> IO ()
unexportEnvs vars = mapM_ (Env.unsetEnv . fst) vars

exportEnvs :: [(String, String)] -> IO ()
exportEnvs vars = mapM_ (\(var, val) -> Env.setEnv var val True) vars

withEnv :: [(String, String)] -> IO a -> IO a
withEnv vars action = do exportEnvs vars
                         result <- action
                         unexportEnvs vars
                         return result
