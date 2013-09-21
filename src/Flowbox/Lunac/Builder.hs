---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Flowbox.Lunac.Builder where

import           Control.Monad.State                                         
import           Data.Maybe                                                  (fromJust)
import           System.TimeIt                                               

import           Flowbox.Prelude                                             
import qualified Flowbox.Luna.Data.AST.Module                              as ASTModule
import qualified Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Lib.Library                                    (Library)
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition)
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager)
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias            as VarAlias
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                       as HSC
import qualified Flowbox.Luna.Passes.General.Luna.Luna                     as Luna
import qualified Flowbox.Luna.Passes.General.Print.Print                   as HSPrint
import qualified Flowbox.Luna.Passes.Transform.AST.GraphParser.GraphParser as GraphParser
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser     as TxtParser
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen        as HASTGen
import qualified Flowbox.Luna.Passes.Transform.Source.Reader.Reader        as SourceReader
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA                     as SSA
import qualified Flowbox.Luna.Passes.Pass                                  as Pass
import           Flowbox.Luna.Passes.Pass                                    (PassMonad)
import           Flowbox.System.Log.Logger                                   
import qualified Flowbox.System.UniPath                                    as UniPath
import           Flowbox.System.UniPath                                      (UniPath)
import qualified Flowbox.Text.Show.Pretty                                  as PP


logger :: Logger
logger = getLogger "Flowbox.Lunac.Builder"


timeLuna :: IO (Either String t) -> IO ()
timeLuna f = do 
    out <- timeIt f
    case out of
        Right _ -> return ()
        Left  e -> fail e


buildLibrary :: Library -> IO ()
buildLibrary library = do
    let defManger = Library.defs library
        rootDefID = Library.rootDefID
        rootDef = fromJust $ DefManager.lab defManger rootDefID
    buildGraph defManger (rootDefID, rootDef)
    

buildGraph :: DefManager -> (Definition.ID, Definition) -> IO ()
buildGraph defManager def = timeLuna $ Luna.run $ do 
    ast <- GraphParser.run defManager def
    logger info  "Running AST"
    logger debug  $ PP.ppShow ast

    buildAST ast


buildFile :: UniPath -> IO ()
buildFile path = timeLuna $ Luna.run $ do 
    source <- SourceReader.run (UniPath.fromUnixString ".") path
    
    logger info "Running TxtParser"
    ast <- TxtParser.run source
    logger debug $ PP.ppqShow ast

    buildAST ast


buildAST :: (MonadIO m, PassMonad s m) => ASTModule.Module -> Pass.Result m ()
buildAST ast = do
    logger info "Running VarAlias"
    va <- VarAlias.run     ast
    logger debug $ PP.ppShow va

    logger info "Running SSA" 
    ssa <- SSA.run va ast
    logger debug $ PP.ppqShow ssa

    logger info "Running HASTGen" 
    hast <- HASTGen.run  ssa
    logger debug $ PP.ppShow hast

    logger info "Running HSC" 
    hsc <- HSC.run hast
    logger debug $ PP.ppShow hsc

    logger info  "Running PHSC" 
    phsc <- HSPrint.run hsc
    logger debug $ phsc

    return ()



