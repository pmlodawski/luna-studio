module Flowbox.RepoManager.Data.Hooks.Install where

import           Flowbox.Prelude
import qualified Control.Exception                                 as Exception
import           Control.Monad                                     (when)
import           Flowbox.RepoManager.Data.Environment              (Command)
import qualified Flowbox.RepoManager.Data.Package.Package          as Package
import qualified Flowbox.RepoManager.Data.Package.Flag             as Flag
import qualified Flowbox.RepoManager.Data.Package.InstalledPackage as InstalledPackage
import qualified Flowbox.RepoManager.Data.RepoConfig               as RepoConfig
import qualified Flowbox.RepoManager.Data.Repository               as Repository
import qualified Flowbox.RepoManager.Data.Version                  as Version
import qualified Flowbox.RepoManager.Utils.Utils                   as Utils
import qualified Data.Digest.Pure.SHA                              as SHA
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
installPackage repo package flags deps = let hash       = InstalledPackage.hashPackage package flags deps
                                             installDir = Repository.config repo ^. RepoConfig.installPath
                                             nameAndVersion = show (package ^. Package.pkgName)
                                                            ++ "-"
                                                            ++ Version.showVersion (package ^. Package.version)
                                             thisPackageDir = installDir FilePath.</> nameAndVersion
                                                            ++ "[" ++ SHA.showDigest hash ++ "]"
                                             resultingPackage = InstalledPackage.makeInstalled package thisPackageDir flags deps hash
                                         in Utils.withDirectory (Repository.config repo ^. RepoConfig.downloadPath) $
                                                -- FIXME[MM]: set installpath to installPath </> qualPkgName-hash
                                                withEnv (prepareEnv repo flags) $ do
                                                    dirExists <- Directory.doesDirectoryExist thisPackageDir
                                                    if dirExists then
                                                            return $ Left "Directory already exists. Probably this package was installed before" 
                                                        else do
                                                            Directory.createDirectoryIfMissing True thisPackageDir
                                                            installExitStatus <- tryExitCode $ runCommands $ package ^. Package.install
                                                            return $ case installExitStatus of
                                                                Left exitCode -> Left $ "Installation script exited with " ++ show exitCode
                                                                _             -> Right resultingPackage

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

prepareEnv :: Repository.Repository a -> [Flag.Flag] -> [(String, String)]
prepareEnv repo flags = paths ++ map (\f -> (Flag.name f, "1")) flags
    where paths = [("DOWNLOAD_PATH", repoConfig ^. RepoConfig.downloadPath),
                   ("MAKE_PATH",     repoConfig ^. RepoConfig.makePath),
                   ("INSTALL_PATH",  repoConfig ^. RepoConfig.installPath)]
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
                                         

--topoSortDependencies :: [Dependency.FixedDependency] -> [Dependency.FixedDependency]
--topoSortDependencies deps = undefined
                                         

--resolveDependencies :: RepoConfig.RepoConfig a -> Package.Package -> [Flag.Flag] -> IO (Either String [Package.Package])
--resolveDependencies config package = undefined
