import Control.Exception (SomeException, try)
import Data.Maybe
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription as PD
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Ar as Ar
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Verbosity
import System.Directory
import System.Environment
import System.FilePath
import System.Process

optimizationLevel = "-O3"
cudaArchitecture  = "sm_30"
cuFiles = unwords [
    "cbits/wrap.cu"
    ]
oName = "lib/foo.o"
--hostCompilerOptions = "-fPIC"

main = defaultMainWithHooks simpleUserHooks {
    hookedPrograms = [simpleProgram "nvcc"],

    confHook = \(description, buildInfo) flags -> do
        localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
        let packageDescription = localPkgDescr localBuildInfo
            library = fromJust $ PD.library packageDescription
            libraryBuildInfo = PD.libBuildInfo library
        dir <- getCurrentDirectory
        return localBuildInfo {
            localPkgDescr = packageDescription {
                PD.library = Just $ library {
                    PD.libBuildInfo = libraryBuildInfo {
                        PD.extraLibDirs = (dir ++ "/lib"):PD.extraLibDirs libraryBuildInfo
                        }
                    }
                }
            },

    buildHook = \pkgDesc localBuildInfo userHooks buildFlags -> do
        let nvccOpts = [optimizationLevel,
                        "-c",
                        "--gpu-architecture=" ++ cudaArchitecture,
                        --"--shared",
                        --"--compiler-options=" ++ hostCompilerOptions,
                        "--output-file=" ++ oName,
                        cuFiles]
        rawSystem "nvcc" nvccOpts

        buildHook simpleUserHooks pkgDesc localBuildInfo userHooks buildFlags

        let verbosity = fromFlag $ buildVerbosity buildFlags
        info verbosity $ "Linking " ++ oName ++ " with Haskell objects"

        withComponentsLBI pkgDesc localBuildInfo $ \comp clbi ->
            case comp of
                CLib lib -> do
                    let pref = buildDir localBuildInfo
                    hobjs <- getHaskellObjects lib localBuildInfo pref objExtension True
                    let cobjs = [oName]
                        staticObjectFiles = hobjs ++ cobjs
                        pkgId = packageId pkgDesc
                        [libName] = componentLibraries clbi
                        vanillaLibFilePath = pref </> mkLibName libName
                    Ar.createArLibArchive verbosity localBuildInfo vanillaLibFilePath staticObjectFiles
                _ -> return ()
    }

-- copy'n'paste from Distribution.Simple.GHC
getHaskellObjects :: Library -> LocalBuildInfo -> FilePath -> String -> Bool -> IO [FilePath]
getHaskellObjects lib lbi pref wanted_obj_ext allow_split_objs
    | splitObjs lbi && allow_split_objs = do
        let splitSuffix = if compilerVersion (compiler lbi) <
                             Version [6, 11] []
                             then "_split"
                             else "_" ++ wanted_obj_ext ++ "_split"
            dirs = [ pref </> (ModuleName.toFilePath x ++ splitSuffix)
                   | x <- libModules lib ]
        objss <- mapM getDirectoryContents dirs
        let objs = [ dir </> obj
                   | (objs',dir) <- zip objss dirs, obj <- objs',
                   let obj_ext = takeExtension obj,
                   '.':wanted_obj_ext == obj_ext ]
        return objs
    | otherwise =
        return [ pref </> ModuleName.toFilePath x <.> wanted_obj_ext
               | x <- libModules lib ]
