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
import qualified Flowbox.Luna.Data.AST.Module                                            as ASTModule
import           Flowbox.Luna.Data.Pass.ASTInfo                                          (ASTInfo)
import           Flowbox.Luna.Data.Pass.Source                                           (Source)
import qualified Flowbox.Luna.Data.Pass.Source                                           as Source
import           Flowbox.Luna.Data.Pass.SourceMap                                        (SourceMap)
import qualified Flowbox.Luna.Passes.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool                          as FuncPool
import           Flowbox.Luna.Passes.Build.BuildConfig                                   (BuildConfig (BuildConfig))
import qualified Flowbox.Luna.Passes.Build.BuildConfig                                   as BuildConfig
import           Flowbox.Luna.Passes.Build.Diagnostics                                   (Diagnostics)
import qualified Flowbox.Luna.Passes.Build.Diagnostics                                   as Diagnostics
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Gen                                   as CabalGen
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Install                               as CabalInstall
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Store                                 as CabalStore
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                                     as HSC
import qualified Flowbox.Luna.Passes.Pass                                                as Pass
import qualified Flowbox.Luna.Passes.Source.File.Reader                                  as FileReader
import qualified Flowbox.Luna.Passes.Source.File.Writer                                  as FileWriter
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Flowbox.Luna.Passes.Transform.AST.Hash.Hash                             as Hash
import qualified Flowbox.Luna.Passes.Transform.AST.Hash.Hash                             as Hash
import qualified Flowbox.Luna.Passes.Transform.AST.SSA.SSA                               as SSA
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser                   as TxtParser
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen                      as HASTGen
import qualified Flowbox.Luna.Passes.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import qualified Flowbox.Luna.Passes.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import           Flowbox.Prelude
import qualified Flowbox.System.Directory.Directory                                      as Directory
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Platform                                                 as Platform
import           Flowbox.System.UniPath                                                  (UniPath)
import qualified Flowbox.System.UniPath                                                  as UniPath
import           Flowbox.Text.Show.Hs                                                    (hsShow)




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


prepareSources :: Diagnostics -> ASTModule.Module -> ASTInfo -> Bool -> Pass.Result [Source]
prepareSources diag ast astInfo implicitSelf = runEitherT $ do
    Diagnostics.printAST ast diag

    -- Should be run BEFORE Analysis.Alias
    (ast, astInfo) <- if implicitSelf
        then do logger debug "\n-------- Desugar.ImplicitSelf --------"
                (ast, astInfo) <- hoistEither =<< Desugar.ImplicitSelf.run astInfo ast
                Diagnostics.printAST ast diag
                return (ast, astInfo)
        else return (ast, astInfo)

    logger debug "\n-------- Desugar.TLRecUpdt --------"
    (ast, astInfo) <- hoistEither =<< Desugar.TLRecUpdt.run astInfo ast
    Diagnostics.printAST ast diag

    logger debug "\n-------- Analysis.Alias --------"
    aliasInfo <- hoistEither =<< Analysis.Alias.run ast
    Diagnostics.printAA aliasInfo diag


    -----------------------------------------
    -- !!! CallGraph and DepSort are mockup passes !!!
    -- They are working right now only with not self typed variables
    logger debug "\n-------- Analysis.CallGraph --------"
    callGraph <- hoistEither =<< Analysis.CallGraph.run aliasInfo ast
    --logger info $ PP.ppShow callGraph


    logger debug "\n-------- Transform.DepSort --------"
    ast <- hoistEither =<< Transform.DepSort.run callGraph aliasInfo ast
    --logger info $ PP.ppShow ast
    -----------------------------------------


    -- !!! [WARNING] INVALIDATES aliasInfo !!!
    logger debug "\n-------- Desugar.ImplicitScopes --------"
    (ast, astInfo) <- hoistEither =<< Desugar.ImplicitScopes.run astInfo aliasInfo ast
    Diagnostics.printAST ast diag

    -- Should be run AFTER ImplicitScopes
    logger debug "\n-------- Desugar.ImplicitCalls --------"
    (ast, astInfo) <- hoistEither =<< Desugar.ImplicitCalls.run astInfo ast
    Diagnostics.printAST ast diag

    logger debug "\n-------- Analysis.Alias --------"
    aliasInfo <- hoistEither =<< Analysis.Alias.run ast
    Diagnostics.printAA aliasInfo diag

    logger debug "\n-------- Hash --------"
    hash <- hoistEither =<< Hash.run ast
    Diagnostics.printHash hash diag

    logger debug "\n-------- SSA --------"
    ssa <- hoistEither =<< SSA.run aliasInfo hash
    Diagnostics.printSSA ssa diag

    logger debug "\n-------- HASTGen --------"
    hast <- hoistEither =<< HASTGen.run ssa
    Diagnostics.printHAST hast diag

    logger debug "\n-------- HSC --------"
    hsc <- hoistEither =<< HSC.run hast
    Diagnostics.printHSC hsc diag

    return $ map formatSource hsc


run :: BuildConfig -> ASTModule.Module -> ASTInfo -> Pass.Result ()
run buildConfig ast astInfo = runEitherT $ do
    let diag    = BuildConfig.diag buildConfig
        allLibs = "base"
                -- : "flowboxM-core"
                : "flowbox-graphics"
                : "luna-target-hs"
                : "template-haskell"
                : "accelerate"
                : BuildConfig.libs buildConfig
                ++ if BuildConfig.name buildConfig /= "flowboxM-stdlib"
                      then ["flowboxM-stdlib"]
                      else []
    hsc <- hoistEither =<< prepareSources diag ast astInfo True
    case BuildConfig.buildDir buildConfig of
        Nothing -> Directory.withTmpDirectory tmpDirPrefix $ buildInFolder buildConfig hsc allLibs
        Just bd -> do liftIO $ Directory.createDirectoryIfMissing True bd
                      buildInFolder buildConfig hsc allLibs bd


buildInFolder :: (Functor m, MonadIO m) => BuildConfig -> [Source] -> [String] -> UniPath -> m ()
buildInFolder (BuildConfig name version _ ghcOptions ccOptions cabalFlags buildType cfg _ _) hsc allLibs buildDir = do
    writeSources buildDir hsc
    let cabal = case buildType of
            BuildConfig.Library       -> CabalGen.genLibrary    name version ghcOptions ccOptions allLibs hsc
            BuildConfig.Executable {} -> CabalGen.genExecutable name version ghcOptions ccOptions allLibs
    CabalStore.run cabal $ UniPath.append (name ++ cabalExt) buildDir
    CabalInstall.run cfg buildDir cabalFlags
    case buildType of
        BuildConfig.Executable outputPath -> copyExecutable buildDir name outputPath
        BuildConfig.Library               -> return ()


writeSources :: (Functor m, MonadIO m) => UniPath -> [Source] -> m ()
writeSources outputPath sources = mapM_ (writeSource outputPath) sources


writeSource :: UniPath -> Source -> Pass.Result ()
writeSource outputPath source = FileWriter.run path hsExt source where
    path     = UniPath.append srcFolder outputPath

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

formatSource :: Source -> Source
formatSource s = s { Source.code = hsShow $ Source.code s }
