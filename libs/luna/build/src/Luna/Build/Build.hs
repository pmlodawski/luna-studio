---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Luna.Build.Build where

import Control.Monad.RWS hiding (mapM, mapM_)

import           Control.Monad.Trans.Either
import           Data.String.Utils                          (replace)
import           Data.Text.Lazy                             (pack, unpack)
import qualified System.Posix.Env                           as Env

import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Flowbox.System.Directory.Directory         as Directory
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Platform                    as Platform
import           Flowbox.System.UniPath                     (UniPath)
import qualified Flowbox.System.UniPath                     as UniPath
import           Luna.Build.BuildConfig                     (BuildConfig (BuildConfig))
import qualified Luna.Build.BuildConfig                     as BuildConfig
import           Luna.Build.Diagnostics                     (Diagnostics, printAST, printHAST, printHSC, printHeader, printSA, printSSA)
import qualified Luna.Build.Source.File                     as File
import qualified Luna.Data.ImportInfo                       as II
import qualified Luna.Pass.Import                           as I
import qualified Luna.Data.ModuleInfo                       as MI
import           Luna.Data.Namespace                        (Namespace (Namespace))
import qualified Luna.Data.Namespace                        as Namespace
import           Luna.Data.Source                           (Code (Code), Source (Source))
import qualified Luna.Data.Source                           as Source
import           Luna.Data.StructData                       (StructData (StructData))
import qualified Luna.Data.StructData                       as StructData
import qualified Luna.Distribution.Cabal.Gen                as CabalGen
import qualified Luna.Distribution.Cabal.Install            as CabalInstall
import qualified Luna.Distribution.Cabal.Store              as CabalStore
import qualified Luna.Parser.Parser                         as Parser
import qualified Luna.Parser.Pragma                         as Pragma
import qualified Luna.Pass                                  as Pass
import qualified Luna.Pass.Analysis.Imports                 as Imports
import qualified Luna.Pass.Analysis.Struct                  as SA
import           Luna.Pass.Import                           (getImportPaths)
import qualified Luna.Pass.Target.HS.HASTGen                as HASTGen
import qualified Luna.Pass.Target.HS.HSC                    as HSC
import qualified Luna.Pass.Transform.Desugar.ImplicitCalls  as ImplCalls
import qualified Luna.Pass.Transform.Desugar.ImplicitScopes as ImplScopes
import qualified Luna.Pass.Transform.Desugar.ImplicitSelf   as ImplSelf
import qualified Luna.Pass.Transform.Parse.Stage1           as Stage1
import qualified Luna.Pass.Transform.Parse.Stage2           as Stage2
import qualified Luna.Pass.Transform.SSA                    as SSA
import           Luna.Syntax.Name.Path                      (QualPath (QualPath))
import qualified Luna.System.Pragma.Store                   as Pragma
import           Luna.System.Session                        as Session
import qualified Luna.Pass.Analysis.InsertStd               as InsertStd


type Builder m = (MonadIO m, Functor m)

logger :: LoggerIO
logger = getLoggerIO $(moduleName)


srcFolder :: String
srcFolder = "src"


hsExt :: String
hsExt = ".hs"


cabalExt :: String
cabalExt = ".cabal"


tmpDirPrefix :: String
tmpDirPrefix = "lunac"

runSession inclStd s =
    case inclStd of
        True  -> eitherStringToM . fst =<< Session.runT (void Parser.init >> Pragma.enable (Pragma.includeStd)  >> Pragma.enable (Pragma.orphanNames) >> runEitherT s)
        False -> eitherStringToM . fst =<< Session.runT (void Parser.init >> Pragma.enable (Pragma.orphanNames) >> runEitherT s)
    

parseSource diag rootSrc src inclStd = do
    let liFile =  src ^. Source.modName

    printHeader "Stage1"
    (ast, astinfo) <- Pass.run1_ Stage1.pass src
    printAST diag ast

    (ast, astinfo) <- Pass.run2_ InsertStd.pass astinfo ast

    let impPaths =  I.getImportPaths ast
    compilable <- liftIO $ filterM MI.moduleExists impPaths
    --putStrLn . show $ compilable
    let mkFile      = Source.File . pack . (++ ".luna") . MI.modPathToString
        sources     = map (\i -> Source i (mkFile i)) compilable
    let hscs        = mapM (prepareSource diag inclStd rootSrc) sources
    compiledCodes <- hscs

    printHeader "Extraction of imports"
    importInfo     <- Pass.run1_ Imports.pass ast


    ----printHeader "Hash"
    ----ast             <- Pass.run1_ Hash.pass ast

    printHeader "SA"
    sa             <- Pass.run2_ SA.pass (StructData mempty importInfo) ast
    let sa1   = sa ^. StructData.namespace . Namespace.info
        mInfo = MI.ModuleInfo liFile mempty sa1 mempty

    liftIO $ MI.writeModInfoToFile mInfo (unpack $ rootSrc ^. Source.src ^. Source.path)
    printSA diag sa1

    printHeader "Stage2"
    (ast, astinfo) <- Pass.run3_ Stage2.pass (Namespace [] sa1) astinfo ast
    printAST diag ast

    printHeader "ImplSelf"
    (ast, astinfo) <- Pass.run2_ ImplSelf.pass astinfo ast
    printAST diag ast

    printHeader "SA2"
    sa             <- Pass.run2_ SA.pass sa ast
    printSA diag (sa ^. StructData.namespace . Namespace.info)

    let sa2 = sa ^. StructData.namespace . Namespace.info
        ii2 = sa ^. StructData.importInfo

    printHeader "ImplScopes"
    (ast, astinfo) <- Pass.run2_ ImplScopes.pass (astinfo, sa2, ii2)  ast
    printAST diag ast

    printHeader "ImplCalls"
    (ast, _astinfo) <- Pass.run2_ ImplCalls.pass astinfo ast
    printAST diag ast
    return (ast, astinfo, importInfo, compiledCodes)


prepareSource :: Builder m => Diagnostics -> Bool -> Source Source.File -> Source Source.File  -> m [Source Code]
prepareSource diag inclStd rootSrc src = do
    codes <- runSession inclStd $ do
        (ast, astinfo, importInfo, compiledCodes) <- parseSource diag rootSrc src inclStd

        printHeader "SSA"
        ast            <- Pass.run1_ SSA.pass ast
        printSSA diag ast

        printHeader "HAST"
        hast           <- Pass.run2_ HASTGen.pass importInfo ast
        printHAST diag hast

        printHeader "HSC"
        hsc            <- Pass.run1_ HSC.pass hast
        printHSC diag hsc

        return (hsc, concat compiledCodes)
    return $ (Source (src ^. Source.modName) $ Code (fst codes)) : (snd codes)


run :: Builder m => BuildConfig -> Diagnostics -> UniPath -> UniPath -> m ()
run buildConfig diag rootPath filePath = do
    lunaPath <- liftIO $ Env.getEnvDefault "LUNAPATH" ""
    let modPath = UniPath.toUnixString $ init filePath
    liftIO $ Env.setEnv "LUNAPATH" (modPath ++ (':' : lunaPath)) True
    case buildConfig ^. BuildConfig.buildDir of
        Nothing -> Directory.withTmpDirectory tmpDirPrefix $ buildInFolder buildConfig diag rootPath filePath
        Just bd -> do liftIO $ Directory.createDirectoryIfMissing True bd
                      buildInFolder buildConfig diag rootPath filePath bd


buildInFolder :: Builder m => BuildConfig -> Diagnostics -> UniPath -> UniPath -> UniPath -> m ()
buildInFolder (BuildConfig name version libs ghcOptions ccOptions includeDirs cabalFlags buildType cfg _ _ inclStd) diag rootPath srcPath buildDir = do
    let allLibs = "base"
                : "luna-target-ghchs"
                : "template-haskell"
                : libs
                ++ ["flowboxM-stdlib" | name /= "flowboxM-stdlib"]
    src <- File.getSource rootPath srcPath
    hsc <- prepareSource diag inclStd src src
    let cabal = case buildType of
            BuildConfig.Executable {} -> CabalGen.genExecutable name version ghcOptions ccOptions includeDirs allLibs
            BuildConfig.Library       -> CabalGen.genLibrary    name version ghcOptions ccOptions includeDirs allLibs hsc
    writeSources buildDir hsc
    CabalStore.run cabal $ UniPath.append (name ++ cabalExt) buildDir
    CabalInstall.run cfg buildDir cabalFlags
    case buildType of
        BuildConfig.Executable outputPath -> copyExecutable buildDir name outputPath
        BuildConfig.Library               -> return ()


writeSources :: Builder m => UniPath -> [Source Code] -> m ()
writeSources outputPath sources = mapM_ (writeSource outputPath) sources


writeSource :: Builder m => UniPath -> Source Code -> m ()
writeSource outputPath source = File.writeSource path hsExt source where
    path     = UniPath.append srcFolder outputPath


copyExecutable :: Builder m => UniPath -> String -> UniPath -> m ()
copyExecutable location name outputPath = liftIO $ do
    let execName   = Platform.dependent name (name ++ ".exe") name
        executable = UniPath.append ("dist/build/" ++ name ++ "/" ++ execName) location
    Directory.copyFile executable outputPath
