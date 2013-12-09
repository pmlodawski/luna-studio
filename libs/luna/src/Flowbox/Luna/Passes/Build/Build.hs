---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Flowbox.Luna.Passes.Build.Build where

import           Control.Applicative                                     
import           Control.Monad.RWS                                     hiding (mapM_, mapM)

import           Flowbox.Prelude                                         
import qualified Flowbox.Luna.Data.AST.Module                          as ASTModule
import qualified Flowbox.Luna.Data.Source                              as Source
import           Flowbox.Luna.Data.Source                                (Source)
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool        as FuncPool
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias        as VarAlias
import qualified Flowbox.Luna.Passes.Build.BuildConfig                 as BuildConfig
import           Flowbox.Luna.Passes.Build.BuildConfig                   (BuildConfig(BuildConfig))
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Gen                 as CabalGen
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Install             as CabalInstall
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Store               as CabalStore
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                   as HSC
import qualified Flowbox.Luna.Passes.Pass                              as Pass
import           Flowbox.Luna.Passes.Pass                                (PassMonadIO)
import qualified Flowbox.Luna.Passes.Source.File.Writer                as FileWriter
import qualified Flowbox.Luna.Passes.Source.File.Reader                as FileReader
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser as TxtParser
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen    as HASTGen
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA                 as SSA
import qualified Flowbox.Luna.Passes.Build.Diagnostics                 as Diagnostics
import qualified Flowbox.System.Directory.Directory                    as Directory
import           Flowbox.System.Log.Logger                               
import qualified Flowbox.System.Platform                               as Platform
import qualified Flowbox.System.UniPath                                as UniPath
import           Flowbox.System.UniPath                                  (UniPath)
import qualified Flowbox.Text.Show.Hs                                  as ShowHs



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Build.Build"


srcFolder :: String
srcFolder = "src"


hsExt :: String
hsExt = ".hs"


cabalExt :: String
cabalExt = ".cabal"


tmpDirPrefix :: String
tmpDirPrefix = "lunac"


run :: PassMonadIO s m => BuildConfig -> ASTModule.Module -> Pass.Result m ()
run (BuildConfig name version libs ghcOptions cabalFlags buildType cfg diag) ast = do
    Diagnostics.printAST ast diag 
    va   <- VarAlias.run ast
    Diagnostics.printVA va diag 
    fp <- FuncPool.run ast
    Diagnostics.printFP fp diag
    ssa  <- SSA.run va ast
    Diagnostics.printSSA ssa diag
    hast <- HASTGen.run ssa fp
    Diagnostics.printHAST hast diag
    hsc  <- map (Source.transCode ShowHs.hsShow) <$> HSC.run hast
    Diagnostics.printHSC hsc diag

    let allLibs = "base"
                : "flowboxM-core"
                : "template-haskell"
                : libs
                ++ if name /= "flowboxM-stdlib"
                      then ["flowboxM-stdlib"]
                      else []

    Directory.withTmpDirectory tmpDirPrefix (\tmpDir -> do
        writeSources tmpDir hsc
        let cabal = case buildType of
                BuildConfig.Library      -> CabalGen.genLibrary    name version ghcOptions allLibs hsc
                BuildConfig.Executable {}-> CabalGen.genExecutable name version ghcOptions allLibs 
        CabalStore.run cabal $ UniPath.append (name ++ cabalExt) tmpDir
        CabalInstall.run cfg tmpDir cabalFlags
        case buildType of
            BuildConfig.Executable outputPath -> copyExecutable tmpDir name outputPath
            BuildConfig.Library               -> return ()
        )


writeSources :: PassMonadIO s m => UniPath -> [Source] -> Pass.Result m ()
writeSources outputPath sources = mapM_ (writeSource outputPath) sources


writeSource :: PassMonadIO s m => UniPath -> Source -> Pass.Result m ()
writeSource outputPath source = FileWriter.run path hsExt source where
    path = UniPath.append srcFolder outputPath

    
copyExecutable :: PassMonadIO s m => UniPath -> String -> UniPath -> Pass.Result m ()
copyExecutable location name outputPath = liftIO $ do 
    let execName   = Platform.dependent name (name ++ ".exe") name
        executable = UniPath.append ("dist/build/" ++ name ++ "/" ++ execName) location
    Directory.copyFile executable outputPath


parseFile :: PassMonadIO s m => UniPath -> UniPath -> Pass.Result m ASTModule.Module
parseFile rootPath filePath = do 
    logger debug $ "Compiling file '" ++ UniPath.toUnixString filePath ++ "'"
    source <- FileReader.run rootPath filePath
    ast    <- TxtParser.run source
    return ast


--parseGraph :: PassMonad s m => Diagnostics -> DefManager -> (Definition.ID, Definition) -> Pass.Result m ASTModule.Module
--parseGraph diag defManager def = do 
--    logger debug "Compiling graph"
--    let tmpFixed_defManager = DequalifyCalls.run defManager
--    Diagnostics.printDM tmpFixed_defManager diag
--    ast <- GraphParser.run tmpFixed_defManager def
--    return ast
