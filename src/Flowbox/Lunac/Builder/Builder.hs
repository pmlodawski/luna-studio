---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Flowbox.Lunac.Builder.Builder where

import           Control.Monad.RWS                                  hiding (mapM_)

import           Flowbox.Prelude                                      
import           Flowbox.Config.Config                                (Config)
import qualified Flowbox.Luna.Data.AST.Module                       as ASTModule
import           Flowbox.Luna.Data.Source                             (Source)
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool     as FuncPool
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias     as VarAlias
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Gen              as CabalGen
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Install          as CabalInstall
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Store            as CabalStore
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                as HSC
import qualified Flowbox.Luna.Passes.Pass                           as Pass
import           Flowbox.Luna.Passes.Pass                             (PassMonadIO)
import qualified Flowbox.Luna.Passes.Source.File.Writer             as FileWriter
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen as HASTGen
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA              as SSA
import qualified Flowbox.Lunac.Diagnostics                          as Diagnostics
import           Flowbox.Lunac.Diagnostics                            (Diagnostics)
import qualified Flowbox.System.Directory.Directory                 as Directory
import           Flowbox.System.Log.Logger                            
import qualified Flowbox.System.UniPath                             as UniPath
import           Flowbox.System.UniPath                               (UniPath)



logger :: Logger
logger = getLogger "Flowbox.Lunac.Builder.Builder"

srcFolder :: String
srcFolder = "src"

hsExt :: String
hsExt = ".hs"

cabalExt :: String
cabalExt = ".cabal"

tmpDirPrefix :: String
tmpDirPrefix = "lunac"


--TODO [PM] : Refactor needed - too many arguments in function
build :: PassMonadIO s m => Config -> Diagnostics -> UniPath -> String -> String -> Bool
      -> [String] -> [String] -> ASTModule.Module -> Pass.Result m ()
build cfg diag outputPath name version isLibrary libs flags ast = do 
    va   <- VarAlias.run ast
    Diagnostics.printVA va diag 
    fp <- FuncPool.run ast
    Diagnostics.printFP fp diag
    ssa  <- SSA.run va ast
    Diagnostics.printSSA ssa diag
    hast <- HASTGen.run ssa fp
    Diagnostics.printHAST hast diag
    hsc  <- HSC.run hast
    Diagnostics.printHSC hsc diag

    let allLibs = "flowboxM-stdlib-io"
                : "template-haskell"
                : libs

    Directory.withTmpDirectory tmpDirPrefix (\tmpDir -> do
        writeSources tmpDir hsc
        let cabal = if isLibrary 
                        then CabalGen.genLibrary hsc name version allLibs         
                        else CabalGen.genExecutable  name version allLibs 
        CabalStore.run cabal $ UniPath.append (name ++ cabalExt) tmpDir
        CabalInstall.run cfg tmpDir flags
        if isLibrary
            then return ()
            else copyExecutable tmpDir name outputPath
        )


writeSources :: PassMonadIO s m => UniPath -> [Source] -> Pass.Result m ()
writeSources outputPath sources = mapM_ (writeSource outputPath) sources


writeSource :: PassMonadIO s m => UniPath -> Source -> Pass.Result m ()
writeSource outputPath source = FileWriter.run path hsExt source where
    path = UniPath.append srcFolder outputPath

    
copyExecutable :: PassMonadIO s m => UniPath -> String -> UniPath -> Pass.Result m ()
copyExecutable location name outputPath = liftIO $ do 
    let executable = UniPath.append ("dist/build/" ++ name ++ "/" ++ name) location
    Directory.copyFile executable outputPath

