---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Flowbox.Lunac.Builder where

import           Control.Monad.RWS                                         hiding (mapM_)
import           Data.Maybe                                                as Maybe
import qualified Data.Set                                                  as Set

import           Flowbox.Prelude                                             
import qualified Flowbox.Initializer.Common                                as Common
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
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool            as FuncPool
import           Flowbox.Luna.Passes.Analysis.FuncPool.Pool                  (Pool(Pool))
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias            as VarAlias
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Install                 as CabalInstall
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Store                   as CabalStore
import qualified Flowbox.Luna.Passes.CodeGen.FClass.Filter                 as FClassFliter
import qualified Flowbox.Luna.Passes.CodeGen.FClass.Gen                    as FClassGen
import qualified Flowbox.Luna.Passes.CodeGen.FClass.Install                as FClassInstall
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                       as HSC
import qualified Flowbox.Luna.Passes.General.Luna.Luna                     as Luna
import qualified Flowbox.Luna.Passes.Pass                                  as Pass
import           Flowbox.Luna.Passes.Pass                                    (PassMonad, PassMonadIO)
import qualified Flowbox.Luna.Passes.Source.File.Reader                    as FileReader
import qualified Flowbox.Luna.Passes.Source.File.Writer                    as FileWriter
import qualified Flowbox.Luna.Passes.Transform.AST.GraphParser.GraphParser as GraphParser
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser     as TxtParser
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen        as HASTGen
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA                     as SSA
import qualified Flowbox.Lunac.Conf                                        as Conf
import           Flowbox.Lunac.Conf                                          (Conf)
import qualified Flowbox.Lunac.Diagnostics                                 as Diagnostics
import           Flowbox.Lunac.Diagnostics                                   (Diagnostics)
import qualified Flowbox.System.Directory.Directory                        as Directory
import           Flowbox.System.Log.Logger                                   
import qualified Flowbox.System.UniPath                                    as UniPath
import           Flowbox.System.UniPath                                      (UniPath)
import qualified Flowbox.Text.Show.Pretty                                  as PP

-- TODO [PM] Split into multiple files: there are too many responsibilities in this file

logger :: Logger
logger = getLogger "Flowbox.Lunac.Builder"


srcFolder :: String
srcFolder = "src"

hsExt :: String
hsExt = ".hs"

cabalExt :: String
cabalExt = ".cabal"


either2io :: IO (Either String a) -> IO a
either2io f = do 
    out <- f
    case out of
        Right r -> return r
        Left  e -> fail e


buildLibrary :: Diagnostics -> Library -> UniPath -> String -> String -> IO ()
buildLibrary diag library outputPath projectName tmpName = either2io $ Luna.run $ do
    let defManger = Library.defs library
        rootDefID = Library.rootDefID
        rootDef = fromJust $ DefManager.lab defManger rootDefID
    ast <- parseGraph diag defManger (rootDefID, rootDef)
    buildAST diag outputPath projectName tmpName ast


buildFile :: Conf -> Diagnostics -> UniPath -> IO ()
buildFile conf diag paths = either2io $ Luna.run $ do 
    let outputPath  = UniPath.fromUnixString $ Conf.output conf
        projectName = Conf.project conf
        tmpName     = "tmp/" ++ projectName
    ast  <- parseFile diag paths
    buildAST diag outputPath projectName tmpName ast


buildAST :: PassMonadIO s m => Diagnostics -> UniPath -> String -> String -> ASTModule.Module -> Pass.Result m ()
buildAST diag outputPath projectName tmpName ast = do 
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
    
    newfp <- FClassFliter.run Common.flowboxPath fp
    FClassInstall.run Common.flowboxPath newfp

    -- CR[wd] czemu funkcja o nazwie buildAST to w ogole robi:
    writeSources tmpName hsc
    runCabal tmpName projectName fp
    moveExecutable tmpName projectName outputPath

    -- CR[wd] czemu funkcja niszczy swoj argument???
    cleanUp tmpName


parseGraph :: PassMonad s m => Diagnostics -> DefManager -> (Definition.ID, Definition) -> Pass.Result m ASTModule.Module
parseGraph diag defManager def = do 
    logger debug "Compiling graph"
    logger info (PP.ppShow  defManager)
    ast <- GraphParser.run defManager def
    Diagnostics.printAST ast diag 
    return ast


parseFile :: PassMonadIO s m => Diagnostics -> UniPath -> Pass.Result m ASTModule.Module
parseFile diag path = do 
    logger debug $ "Compiling file '" ++ UniPath.toUnixString path ++ "'"
    let rootPath = UniPath.basePath path
    source <- FileReader.run rootPath path
    ast    <- TxtParser.run source
    Diagnostics.printAST ast diag 
    return ast


genCabal :: String -> Pool -> CabalConfig.Config
genCabal name (Pool names) = let
    exec_base = CabalSection.mkExecutable name 
    exec = exec_base { CabalSection.buildDepends = ["pretty-show"
                                                   , "random"
                                                   , "base"
                                                   , "OneTuple"
                                                   , "template-haskell"
                                                   , "flowboxM-stdlib-io"
                                                   ] ++ (map FClassGen.packageName $ Set.toList names)
                     }

    -- TODO [PM] : refactor. mkExecutable silently creates project with MainIs = "Main.hs" and hsSourceDirs = "src"
    conf = CabalConfig.addSection exec 
         $ CabalConfig.make name
    in conf


writeSources :: PassMonadIO s m => String -> [Source] -> Pass.Result m ()
writeSources location sources = do 
    let outputPath = UniPath.append location Common.flowboxPath
    mapM_ (FileWriter.run (UniPath.append srcFolder outputPath) hsExt) sources -- CR[wd]: ta linijka jest niezrozumiala


runCabal :: PassMonadIO s m => String -> String -> Pool -> Pass.Result m ()
runCabal location name pool = do 
    let cabal      = genCabal name pool
        outputPath = UniPath.append location Common.flowboxPath
    CabalStore.run cabal $ UniPath.append (name ++ cabalExt) outputPath
    CabalInstall.run Common.flowboxPath location


moveExecutable :: PassMonadIO s m => String -> String -> UniPath -> Pass.Result m ()
moveExecutable location name outputPath = liftIO $ do 
    rootPath      <- UniPath.expand $ UniPath.append location Common.flowboxPath
    let executable = UniPath.append ("dist/build/" ++ name ++ "/" ++ name) rootPath
    Directory.renameFile executable outputPath


cleanUp :: PassMonadIO s m => String -> Pass.Result m ()
cleanUp location = liftIO $ do 
    outputPath <- UniPath.expand $ UniPath.append location Common.flowboxPath
    Directory.removeDirectoryRecursive outputPath
