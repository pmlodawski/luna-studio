---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Flowbox.Lunac.Builder where

import           Control.Monad.RWS                                         hiding (mapM_)
import           Data.Maybe                                                  (fromJust)

import           Flowbox.Prelude                                             
import qualified Flowbox.Luna.Data.AST.Module                              as ASTModule
import qualified Flowbox.Luna.Data.Cabal.Config                            as CabalConfig
import qualified Flowbox.Luna.Data.Cabal.Section                           as CabalSection
import           Flowbox.Luna.Data.Source                                    (Source)
import qualified Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Lib.Library                                    (Library)
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition)
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager)
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias            as VarAlias
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Store                   as CabalStore
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                       as HSC
import qualified Flowbox.Luna.Passes.General.Luna.Luna                     as Luna
import qualified Flowbox.Luna.Passes.Pass                                  as Pass
import           Flowbox.Luna.Passes.Pass                                    (PassMonadIO)
import qualified Flowbox.Luna.Passes.Transform.AST.GraphParser.GraphParser as GraphParser
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser     as TxtParser
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen        as HASTGen
import qualified Flowbox.Luna.Passes.Source.File.Reader                    as FileReader
import qualified Flowbox.Luna.Passes.Source.File.Writer                    as FileWriter
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA                     as SSA
import qualified Flowbox.Lunac.Diagnostics                                 as Diagnostics
import           Flowbox.Lunac.Diagnostics                                   (Diagnostics)
import           Flowbox.System.Log.Logger                                   
import qualified Flowbox.System.UniPath                                    as UniPath
import           Flowbox.System.UniPath                                      (UniPath)
import qualified Flowbox.System.Directory                                  as Directory
import qualified Flowbox.System.Process                                    as Process


logger :: Logger
logger = getLogger "Flowbox.Lunac.Builder"


either2io :: IO (Either String a) -> IO a
either2io f = do 
    out <- f
    case out of
        Right r -> return r
        Left  e -> fail e


buildLibrary :: Diagnostics -> Library -> IO [Source]
buildLibrary diag library = do
    let defManger = Library.defs library
        rootDefID = Library.rootDefID
        rootDef = fromJust $ DefManager.lab defManger rootDefID
    buildGraph diag defManger (rootDefID, rootDef)
    

buildGraph :: Diagnostics -> DefManager -> (Definition.ID, Definition) -> IO [Source]
buildGraph diag defManager def = either2io $ Luna.run $ do 
    logger debug "Compiling graph"
    ast <- GraphParser.run defManager def
    Diagnostics.printAST ast diag 
    buildAST diag ast


buildFile :: Diagnostics -> UniPath -> IO [Source]
buildFile diag path = either2io $ Luna.run $ do 
    logger debug $ "Compiling file '" ++ UniPath.toUnixString path ++ "'"
    let rootPath = UniPath.basePath path
    source <- FileReader.run rootPath path
    ast    <- TxtParser.run source
    Diagnostics.printAST ast diag 
    buildAST diag ast


buildAST :: PassMonadIO s m => Diagnostics -> ASTModule.Module -> Pass.Result m [Source]
buildAST diag ast = do
    va   <- VarAlias.run ast
    Diagnostics.printVA va diag 
    ssa  <- SSA.run va ast
    Diagnostics.printSSA ssa diag
    hast <- HASTGen.run ssa
    Diagnostics.printHAST hast diag
    hsc  <- HSC.run hast
    Diagnostics.printHSC hsc diag
    return hsc


srcFolder :: String
srcFolder = "src"

hsExt :: String
hsExt = ".hs"

cabalExt :: String
cabalExt = ".cabal"


genCabal :: String -> CabalConfig.Config
genCabal name = let
    exec_base = CabalSection.mkExecutable name 
    exec = exec_base { CabalSection.buildDepends = ["pretty-show"
                                                   , "random"
                                                   , "base"
                                                   , "OneTuple"
                                                   , "template-haskell"
                                                   , "flowboxM-stdlib-io"
                                                   ]
                     }

    -- TODO [PM] : refactor. mkExecutable silently creates project with MainIs = "Main.hs" and hsSourceDirs = "src"
    conf = CabalConfig.addSection exec 
         $ CabalConfig.make name
    in conf


flowboxPath :: UniPath
flowboxPath = UniPath.fromUnixString "~/.flowbox"


initializeCabalDev :: IO ()
initializeCabalDev = do
    Directory.createDirectoryIfMissing True $ flowboxPath
    Process.runCommandInFolder flowboxPath "cabal-dev" ["update"] 


buildSources :: String -> [Source] -> IO ()
buildSources location sources = either2io $ Luna.run $ do 
    let outputPath = UniPath.append location flowboxPath
    mapM_ (FileWriter.run (UniPath.append srcFolder outputPath) hsExt) sources 


runCabal :: String -> String -> IO ()
runCabal location name = either2io $ Luna.run $ do 
    let cabal      = genCabal name
        outputPath = UniPath.append location flowboxPath

    CabalStore.run cabal $ UniPath.append (name ++ cabalExt) outputPath
    liftIO $ Process.runCommandInFolder flowboxPath "cabal-dev" ["install", location, "--reinstall"] 


moveExecutable :: String -> String -> UniPath -> IO ()
moveExecutable location name outputPath = do 
    rootPath      <- UniPath.expand $ UniPath.append location flowboxPath
    let executable = UniPath.append ("dist/build/" ++ name ++ "/" ++ name) rootPath
    Directory.renameFile (UniPath.toUnixString executable) (UniPath.toUnixString outputPath)


cleanUp :: String -> IO ()
cleanUp location = do 
    outputPath <- UniPath.expand $ UniPath.append location flowboxPath
    Directory.removeDirectoryRecursive $ UniPath.toUnixString outputPath
