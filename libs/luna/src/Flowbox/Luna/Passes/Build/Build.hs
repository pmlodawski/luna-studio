---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}

module Flowbox.Luna.Passes.Build.Build where

import Control.Applicative
import Control.Monad.RWS   hiding (mapM, mapM_)

import           Control.Monad.Trans.Either
import qualified Flowbox.Luna.Data.AST.Module                          as ASTModule
import           Flowbox.Luna.Data.Pass.ASTInfo                        (ASTInfo)
import           Flowbox.Luna.Data.Pass.Source                         (Source)
import qualified Flowbox.Luna.Data.Pass.Source                         as Source
import           Flowbox.Luna.Data.Pass.SourceMap                      (SourceMap)
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool        as FuncPool
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias        as VarAlias
import           Flowbox.Luna.Passes.Build.BuildConfig                 (BuildConfig (BuildConfig))
import qualified Flowbox.Luna.Passes.Build.BuildConfig                 as BuildConfig
import qualified Flowbox.Luna.Passes.Build.Diagnostics                 as Diagnostics
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Gen                 as CabalGen
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Install             as CabalInstall
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Store               as CabalStore
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                   as HSC
import qualified Flowbox.Luna.Passes.Pass                              as Pass
import qualified Flowbox.Luna.Passes.Source.File.Reader                as FileReader
import qualified Flowbox.Luna.Passes.Source.File.Writer                as FileWriter
import qualified Flowbox.Luna.Passes.Transform.AST.Hash.Hash           as Hash
import qualified Flowbox.Luna.Passes.Transform.AST.SSA.SSA             as SSA
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser as TxtParser
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen    as HASTGen
import           Flowbox.Prelude
import qualified Flowbox.System.Directory.Directory                    as Directory
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Platform                               as Platform
import           Flowbox.System.UniPath                                (UniPath)
import qualified Flowbox.System.UniPath                                as UniPath
import qualified Flowbox.Text.Show.Hs                                  as ShowHs


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.Build.Build"


srcFolder :: String
srcFolder = "src"


hsExt :: String
hsExt = ".hs"


cabalExt :: String
cabalExt = ".cabal"


tmpDirPrefix :: String
tmpDirPrefix = "lunac"


run :: BuildConfig -> ASTModule.Module -> Pass.Result ()
run buildConfig ast = runEitherT $ do
    let diag = BuildConfig.diag buildConfig
    Diagnostics.printAST ast diag
    va   <- hoistEither =<< VarAlias.run ast
    Diagnostics.printVA va diag
    fp   <- hoistEither =<< FuncPool.run ast
    Diagnostics.printFP fp diag
    hash <- hoistEither =<< Hash.run ast
    -- TODO Diagnostics
    ssa  <- hoistEither =<< SSA.run va hash
    Diagnostics.printSSA ssa diag
    hast <- hoistEither =<< HASTGen.run ssa fp
    Diagnostics.printHAST hast diag
    hsc  <- map (Source.transCode ShowHs.hsShow) <$> (hoistEither =<< HSC.run hast)
    Diagnostics.printHSC hsc diag

    let allLibs = "base"
                -- : "flowboxM-core"
                : "flowbox-graphics"
                : "luna-target-hs"
                : "template-haskell"
                : BuildConfig.libs buildConfig
                ++ if BuildConfig.name buildConfig /= "flowboxM-stdlib"
                      then ["flowboxM-stdlib"]
                      else []
    case BuildConfig.buildDir buildConfig of
        Nothing -> Directory.withTmpDirectory tmpDirPrefix $ buildInFolder buildConfig hsc allLibs
        Just bd -> do liftIO $ Directory.createDirectoryIfMissing True bd
                      buildInFolder buildConfig hsc allLibs bd

buildInFolder :: (Functor m, MonadIO m) => BuildConfig -> [Source] -> [String] -> UniPath -> m ()
buildInFolder (BuildConfig name version _ ghcOptions cppOptions cabalFlags buildType cfg _ _) hsc allLibs buildDir = do
    writeSources buildDir hsc
    let cabal = case buildType of
            BuildConfig.Library       -> CabalGen.genLibrary    name version ghcOptions cppOptions allLibs hsc
            BuildConfig.Executable {} -> CabalGen.genExecutable name version ghcOptions cppOptions allLibs
    CabalStore.run cabal $ UniPath.append (name ++ cabalExt) buildDir
    CabalInstall.run cfg buildDir cabalFlags
    case buildType of
        BuildConfig.Executable outputPath -> copyExecutable buildDir name outputPath
        BuildConfig.Library               -> return ()

writeSources :: (Functor m, MonadIO m) => UniPath -> [Source] -> m ()
writeSources outputPath sources = mapM_ (writeSource outputPath) sources


writeSource :: UniPath -> Source -> Pass.Result ()
writeSource outputPath source = FileWriter.run path hsExt source where
    path = UniPath.append srcFolder outputPath


copyExecutable :: MonadIO m => UniPath -> String -> UniPath -> m ()
copyExecutable location name outputPath = liftIO $ do
    let execName   = Platform.dependent name (name ++ ".exe") name
        executable = UniPath.append ("dist/build/" ++ name ++ "/" ++ execName) location
    Directory.copyFile executable outputPath


parseFile :: UniPath -> UniPath -> Pass.Result (ASTModule.Module, SourceMap, ASTInfo)
parseFile rootPath filePath = runEitherT $ do
    logger debug $ "Compiling file '" ++ UniPath.toUnixString filePath ++ "'"
    source <- hoistEither =<< FileReader.run rootPath filePath
    result <- hoistEither =<< TxtParser.run source
    return result
