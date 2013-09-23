---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Flowbox.Lunac.Builder where

import           Data.Maybe                                                  (fromJust)

import           Flowbox.Prelude                                             
import qualified Flowbox.Luna.Data.AST.Module                              as ASTModule
import qualified Flowbox.Luna.Data.Source                                  as Source
import           Flowbox.Luna.Data.Source                                    (Source(Source))
import qualified Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Lib.Library                                    (Library)
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition)
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager)
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias            as VarAlias
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Build.CabalBuild        as CabalBuild
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Gen.Defaults            as CabalDefaults
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Store.CabalStore        as CabalStore
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                       as HSC
import qualified Flowbox.Luna.Passes.General.Luna.Luna                     as Luna
import qualified Flowbox.Luna.Passes.Pass                                  as Pass
import           Flowbox.Luna.Passes.Pass                                    (PassMonadIO)
import qualified Flowbox.Luna.Passes.Transform.AST.GraphParser.GraphParser as GraphParser
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser     as TxtParser
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen        as HASTGen
import qualified Flowbox.Luna.Passes.Source.File.Reader.Reader             as FileReader
import qualified Flowbox.Luna.Passes.Source.File.Writer.Writer             as FileWriter
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA                     as SSA
import qualified Flowbox.Lunac.Diagnostics                                 as Diagnostics
import           Flowbox.Lunac.Diagnostics                                   (Diagnostics(Diagnostics))
import           Flowbox.System.Log.Logger                                   
import qualified Flowbox.System.UniPath                                    as UniPath
import           Flowbox.System.UniPath                                      (UniPath)


logger :: Logger
logger = getLogger "Flowbox.Lunac.Builder"


either2io :: IO (Either String a) -> IO a
either2io f = do 
    out <- f
    case out of
        Right r -> return r
        Left  e -> fail e


buildLibrary :: Library -> IO [Source]
buildLibrary library = do
    let diag = Diagnostics False False False False False -- TODO[PM] : remove; added to fix compilation errors
        defManger = Library.defs library
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

launcher :: Source 
launcher = Source  ["Launcher"]
         $ unlines [ "import Main as M"
                   , "main = M.main 0"]



buildSources :: UniPath -> [Source] -> IO ()
buildSources outputPath sources = either2io $ Luna.run $ do 
    mapM_ (FileWriter.run (UniPath.append srcFolder outputPath) hsExt) $ launcher : sources 


runCabal :: UniPath -> String -> IO ()
runCabal path name = either2io $ Luna.run $ do 
    let mainIs = UniPath.setExtension hsExt $ UniPath.fromList $ Source.path launcher
        cabal = CabalDefaults.defaultConfig name [UniPath.fromUnixString srcFolder] mainIs
    CabalStore.run cabal $ UniPath.append (name ++ cabalExt) path
    CabalBuild.run path
    
