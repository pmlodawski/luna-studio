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
import qualified Flowbox.Luna.Data.AST.Expr                                as ASTExpr
import qualified Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Lib.Library                                    (Library)
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition)
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager)
import qualified Flowbox.Luna.Passes.Transform.AST.GraphParser.GraphParser as GraphParser
import qualified Flowbox.Luna.Passes.Transform.HS.HASTGen.HASTGen          as HASTGen
import qualified Flowbox.Luna.Passes.Transform.HS.CodeGen.CodeGen          as CodeGen
import qualified Flowbox.Luna.Passes.Transform.HS.Print.Print              as HSPrint
import qualified Flowbox.Luna.Passes.General.Luna.Luna                     as Luna
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA                     as SSA
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser     as TxtParser
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias            as VarAlias
import qualified Flowbox.Luna.Passes.Pass                                  as Pass
import           Flowbox.Luna.Passes.Pass                                    (PassMonad)
import           Flowbox.System.Log.Logger                                   
import qualified Flowbox.Text.Show.Pretty                                  as PP


logger :: Logger
logger = getLogger "Flowbox.Lunac.Builder"


buildLibrary :: Library -> IO ()
buildLibrary library = do
    let defManger = Library.defs library
        rootDefID = Library.rootDefID
        rootDef = fromJust $ DefManager.lab defManger rootDefID
    out <- timeIt $ buildGraph defManger (rootDefID, rootDef)
    case out of
        Right _ -> return ()
        Left  e -> fail e


buildGraph :: DefManager -> (Definition.ID, Definition) -> IO (Either String ())
buildGraph defManager def = Luna.run $ do 
    ast <- GraphParser.run defManager def
    putStrLn "\n-------- AST --------"
    putStrLn $ PP.ppShow ast

    buildAST ast


buildAST :: (MonadIO m, PassMonad s m) => ASTExpr.Expr -> Pass.Result m ()
buildAST (ast :: ASTExpr.Expr) = do
    putStrLn "\n-------- VarAlias --------"
    va <- VarAlias.run     ast
    putStrLn $ PP.ppShow va

    putStrLn "\n-------- SSA --------" 
    ssa <- SSA.run va ast
    putStrLn $ PP.ppqShow ssa

    putStrLn "\n-------- HASTGen --------" 
    hast <- HASTGen.run  ssa
    putStrLn $ PP.ppShow hast

    --putStrLn "\n-------- HS CodeGen --------" 
    hsc <- CodeGen.run  hast
    --putStrLn $ hsc

    putStrLn "\n-------- PHSC --------" 
    phsc <- HSPrint.run hsc
    putStrLn $ phsc


    return ()



