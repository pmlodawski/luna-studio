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
import qualified Flowbox.Luna.Passes.Transform.AST.GraphParser.GraphParser as GraphParser
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser     as TxtParser
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen        as HASTGen
import qualified Flowbox.Luna.Passes.Source.FileReader.FileReader          as FileReader
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA                     as SSA
import qualified Flowbox.Luna.Passes.Pass                                  as Pass
import           Flowbox.Luna.Passes.Pass                                    (PassMonad)
import           Flowbox.System.Log.Logger                                   
import qualified Flowbox.System.UniPath                                    as UniPath
import           Flowbox.System.UniPath                                      (UniPath)
import qualified Flowbox.Text.Show.Pretty                                  as PP
import qualified Flowbox.Lunac.Diagnostics                                 as Diagnostics
import           Flowbox.Lunac.Diagnostics                                   (Diagnostics(Diagnostics))


logger :: Logger
logger = getLogger "Flowbox.Lunac.Builder"


hoistLuna :: IO (Either String t) -> IO ()
hoistLuna f = do 
    out <- f
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
buildGraph defManager def = hoistLuna $ Luna.run $ do 
    logger debug "Compiling graph"
    ast <- GraphParser.run defManager def
    buildAST ast


buildFile :: Diagnostics -> UniPath -> IO ()
buildFile diag path = hoistLuna $ Luna.run $ do 
    logger debug $ "Compiling file '" ++ UniPath.toUnixString path ++ "'"
    source <- FileReader.run (UniPath.fromUnixString ".") path
    ast    <- TxtParser.run source
    Diagnostics.printAST ast diag -- TODO[wd]: Diagnostyka powinna byc przekazywana dalej.
    buildAST ast


buildAST :: (MonadIO m, PassMonad s m) => ASTModule.Module -> Pass.Result m ()
buildAST ast = do
    va   <- VarAlias.run ast
    ssa  <- SSA.run va ast
    hast <- HASTGen.run ssa
    hsc  <- HSC.run hast

    return ()



