---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Flowbox.Lunac.Builder where

import           Control.Monad.RWS                                         hiding (mapM_)
import qualified Data.Maybe                                                as Maybe
import qualified Data.Set                                                  as Set

import           Flowbox.Prelude                                             
import qualified Flowbox.Config.Config                                     as Config
import           Flowbox.Config.Config                                       (Config)
import qualified Flowbox.Luna.Data.AST.Module                              as ASTModule
import           Flowbox.Luna.Data.Source                                    (Source)
import qualified Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Lib.Library                                    (Library)
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition)
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager)
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool            as FuncPool
import           Flowbox.Luna.Passes.Analysis.FuncPool.Pool                as Pool
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias            as VarAlias
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Gen                     as CabalGen
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
import qualified Flowbox.Lunac.CmdArgs                                     as CmdArgs
import           Flowbox.Lunac.CmdArgs                                       (CmdArgs)
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


-- TODO [PM] : Refactor needed
getBuildPath :: Config -> UniPath
getBuildPath = UniPath.fromUnixString . Config.path . Config.local 


-- TODO [PM] Refactor needed 
buildLibrary :: Config -> Diagnostics -> Library -> String -> UniPath -> IO ()
buildLibrary config diag library name outputPath = Luna.runIO $ do
    let defManger = Library.defs library
        rootDefID = Library.rootDefID
        rootDef = Maybe.fromJust $ DefManager.lab defManger rootDefID
    ast <- parseGraph diag defManger (rootDefID, rootDef)
    buildAST config diag outputPath name False [] ast


-- TODO [PM] Refactor needed 
buildFile :: Config -> CmdArgs -> Diagnostics -> UniPath -> IO ()
buildFile config cmd diag path = Luna.runIO $ do 
    let outputPath = UniPath.fromUnixString $ CmdArgs.output cmd
        name       = CmdArgs.name cmd
        rootPath = case CmdArgs.rootPath cmd of 
                        "" -> UniPath.basePath path
                        a  -> UniPath.fromUnixString a

    ast  <- parseFile diag rootPath path
    buildAST config diag outputPath name (CmdArgs.library cmd) (CmdArgs.link cmd) ast


notImplementedList = []

-- TODO [PM] Refactor needed 
buildAST :: PassMonadIO s m => Config -> Diagnostics -> UniPath -> String -> Bool -> [String] -> ASTModule.Module -> Pass.Result m ()
buildAST config diag outputPath name isLibrary libs ast = do 
    let flags = notImplementedList
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
    newfp <- FClassFliter.run config fp
    FClassInstall.run config newfp flags

    let allLibs = libs ++ (map FClassGen.packageName $ Set.toList $ Pool.names fp)

    Directory.withTmpDirectory "lunac" (\tmpDir -> do
        writeSources tmpDir hsc
        let cabal = if isLibrary 
                        then CabalGen.genLibrary hsc name allLibs         
                        else CabalGen.genExecutable  name allLibs 
        CabalStore.run cabal $ UniPath.append (name ++ cabalExt) tmpDir
        CabalInstall.run config tmpDir flags
        if isLibrary
            then return ()
            else copyExecutable tmpDir name outputPath
        )
    


parseGraph :: PassMonad s m => Diagnostics -> DefManager -> (Definition.ID, Definition) -> Pass.Result m ASTModule.Module
parseGraph diag defManager def = do 
    logger debug "Compiling graph"
    logger info (PP.ppShow  defManager)
    ast <- GraphParser.run defManager def
    Diagnostics.printAST ast diag 
    return ast


parseFile :: PassMonadIO s m => Diagnostics -> UniPath -> UniPath -> Pass.Result m ASTModule.Module
parseFile diag rootPath path = do 
    logger debug $ "Compiling file '" ++ UniPath.toUnixString path ++ "'"
    source <- FileReader.run rootPath path
    ast    <- TxtParser.run source
    Diagnostics.printAST ast diag 
    return ast


writeSources :: PassMonadIO s m => UniPath -> [Source] -> Pass.Result m ()
writeSources outputPath sources = mapM_ writeSource sources where
    writeSource :: PassMonadIO s m => Source -> Pass.Result m ()
    writeSource source = FileWriter.run path hsExt source where
        path = UniPath.append srcFolder outputPath

    
copyExecutable :: PassMonadIO s m => UniPath -> String -> UniPath -> Pass.Result m ()
copyExecutable location name outputPath = liftIO $ do 
    let executable = UniPath.append ("dist/build/" ++ name ++ "/" ++ name) location
    Directory.copyFile executable outputPath

