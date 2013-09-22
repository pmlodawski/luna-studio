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

import           Flowbox.Prelude                                             
import qualified Flowbox.Luna.Data.AST.Module                              as ASTModule
import           Flowbox.Luna.Data.Source                                    (Source)
import qualified Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Lib.Library                                    (Library)
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition)
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager)
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias            as VarAlias
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                       as HSC
import qualified Flowbox.Luna.Passes.General.Luna.Luna                     as Luna
import qualified Flowbox.Luna.Passes.Transform.AST.GraphParser.GraphParser as GraphParser
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser     as TxtParser
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen        as HASTGen
import qualified Flowbox.Luna.Passes.Source.FileReader.FileReader          as FileReader
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA                     as SSA
import qualified Flowbox.Luna.Passes.Pass                                  as Pass
import           Flowbox.Luna.Passes.Pass                                    (PassMonad)
import qualified Flowbox.Lunac.Diagnostics                                 as Diagnostics
import           Flowbox.Lunac.Diagnostics                                   (Diagnostics)
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
    -- TODO[wd]: "path" in the following line should point relatively to the main file.
    source <- FileReader.run (UniPath.fromUnixString ".") path
    ast    <- TxtParser.run source
    Diagnostics.printAST ast diag 
    buildAST diag ast


buildAST :: (MonadIO m, PassMonad s m) => Diagnostics -> ASTModule.Module -> Pass.Result m [Source]
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



