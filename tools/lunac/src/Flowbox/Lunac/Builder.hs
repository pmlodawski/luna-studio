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
import qualified Data.List                                                 as List
import qualified Data.Set                                                  as Set

import           Flowbox.Prelude                                             
import qualified Flowbox.Config.Config                                     as Config
import           Flowbox.Config.Config                                       (Config)
import qualified Flowbox.Luna.Data.AST.Module                              as ASTModule
import qualified Flowbox.Luna.Data.Cabal.Config                            as CabalConfig
import qualified Flowbox.Luna.Data.Cabal.Section                           as CabalSection
import qualified Flowbox.Luna.Data.Source                                  as Source
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


either2io :: IO (Either String a) -> IO a
either2io f = do 
    out <- f
    case out of
        Right r -> return r
        Left  e -> fail e


-- TODO [PM] : Refactor needed
getBuildPath :: Config -> UniPath
getBuildPath = UniPath.fromUnixString . Config.path . Config.usr 


-- TODO [PM] Refactor needed 
buildLibrary :: Config -> Diagnostics -> Library -> UniPath -> String -> String -> IO ()
buildLibrary config diag library outputPath name tmpName = either2io $ Luna.run $ do
    let defManger = Library.defs library
        rootDefID = Library.rootDefID
        rootDef = Maybe.fromJust $ DefManager.lab defManger rootDefID
    ast <- parseGraph diag defManger (rootDefID, rootDef)
    buildAST config diag outputPath name tmpName False [] ast


-- TODO [PM] Refactor needed 
buildFile :: Config -> CmdArgs -> Diagnostics -> UniPath -> IO ()
buildFile config cmd diag path = either2io $ Luna.run $ do 
    let outputPath = UniPath.fromUnixString $ CmdArgs.output cmd
        name       = CmdArgs.name cmd
        tmpName    = "tmp/" ++ name
        
        rootPath = case CmdArgs.rootPath cmd of 
                        "" -> UniPath.basePath path
                        a  -> UniPath.fromUnixString a

    ast  <- parseFile diag rootPath path
    buildAST config diag outputPath name tmpName (CmdArgs.library cmd) (CmdArgs.link cmd) ast


-- TODO [PM] Refactor needed 
buildAST :: PassMonadIO s m => Config -> Diagnostics -> UniPath -> String -> String -> Bool -> [String] -> ASTModule.Module -> Pass.Result m ()
buildAST config diag outputPath name tmpName isLibrary libs ast = do 
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
    
    newfp <- FClassFliter.run (getBuildPath config) fp
    FClassInstall.run (getBuildPath config) newfp

    let cabal    = genCabal name isLibrary hsc fp libs 
    -- CR[wd] czemu funkcja o nazwie buildAST to w ogole robi:
    writeSources config tmpName hsc
    runCabal config tmpName name cabal
    if isLibrary
        then return ()
        else moveExecutable config tmpName name outputPath
    -- CR[wd] czemu funkcja niszczy swoj argument???
    cleanUp config tmpName


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




-- TODO [PM] Refactor needed 
genCabal :: String -> Bool -> [Source] -> Pool -> [String] -> CabalConfig.Config
genCabal name isLibrary sources (Pool names) libs  = conf where
    getModuleName :: Source -> String
    getModuleName source = List.intercalate "." $ Source.path source

    section_base = if isLibrary 
                     then CabalSection.mkLibrary { CabalSection.exposedModules = map getModuleName sources }
                     else CabalSection.mkExecutable name 
    section = section_base { CabalSection.buildDepends = "pretty-show"
                                                       : "random"
                                                       : "base"
                                                       : "OneTuple"
                                                       : "template-haskell"
                                                       : "flowboxM-stdlib-io"
                                                       :  (map FClassGen.packageName $ Set.toList names)
                                                       ++ libs
                           }
    conf = CabalConfig.addSection section 
         $ CabalConfig.make name


writeSources :: PassMonadIO s m => Config -> String -> [Source] -> Pass.Result m ()
writeSources config location sources = do 
    let outputPath = UniPath.append location (getBuildPath config)

        writeSource :: PassMonadIO s m => Source -> Pass.Result m ()
        writeSource source = do 
            let path = UniPath.append srcFolder outputPath
            FileWriter.run path hsExt source

    mapM_ writeSource sources


runCabal :: PassMonadIO s m => Config -> String -> String -> CabalConfig.Config -> Pass.Result m ()
runCabal config location name cabal = do 
    let outputPath = UniPath.append location (getBuildPath config)
    CabalStore.run cabal $ UniPath.append (name ++ cabalExt) outputPath
    CabalInstall.run (getBuildPath config) location


moveExecutable :: PassMonadIO s m => Config -> String -> String -> UniPath -> Pass.Result m ()
moveExecutable config location name outputPath = liftIO $ do 
    rootPath      <- UniPath.expand $ UniPath.append location (getBuildPath config)
    let executable = UniPath.append ("dist/build/" ++ name ++ "/" ++ name) rootPath
    Directory.renameFile executable outputPath


cleanUp :: PassMonadIO s m => Config -> String -> Pass.Result m ()
cleanUp config location = liftIO $ do 
    outputPath <- UniPath.expand $ UniPath.append location (getBuildPath config)
    Directory.removeDirectoryRecursive outputPath
