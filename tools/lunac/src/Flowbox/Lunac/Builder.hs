---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Flowbox.Lunac.Builder where

import           Control.Monad.State                       
import           Data.Maybe                                (fromJust)
import           System.TimeIt                             

import           Flowbox.Prelude                           
import qualified Flowbox.Luna.AST.Expr                   as ASTExpr
import qualified Flowbox.Luna.Lib.Library                as Library
import           Flowbox.Luna.Lib.Library                  (Library)
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Def.Definition       (Definition)
import qualified Flowbox.Luna.Network.Def.DefManager     as DefManager
import           Flowbox.Luna.Network.Def.DefManager       (DefManager)
import qualified Flowbox.Luna.Passes.Graph2AST.Graph2AST as Graph2AST
import qualified Flowbox.Luna.Passes.HSGen.HSC           as HSC
import qualified Flowbox.Luna.Passes.HSGen.HSGen         as HSGen
import qualified Flowbox.Luna.Passes.Luna.Luna           as Luna
import qualified Flowbox.Luna.Passes.Pass                as Pass
import           Flowbox.Luna.Passes.Pass                  (PassMonad)
import qualified Flowbox.Luna.Passes.SSA.SSA             as SSA
import qualified Flowbox.Luna.Passes.VA.VA               as VA
import           Flowbox.System.Log.Logger                 
import qualified Flowbox.Text.Show.Pretty                as PP



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
    ast <- Graph2AST.run defManager def
    putStrLn "\n-------- AST --------"
    putStrLn $ PP.ppShow ast

    buildAST ast


buildAST :: (MonadIO m, PassMonad s m) => ASTExpr.Expr -> Pass.Result m ()
buildAST (ast :: ASTExpr.Expr) = do
    putStrLn "\n-------- VA --------"
    va <- VA.run     ast
    putStrLn $ PP.ppShow va

    putStrLn "\n-------- SSA --------" 
    ssa <- SSA.run va ast
    putStrLn $ PP.ppqShow ssa

    putStrLn "\n-------- HSGen --------" 
    hast <- HSGen.run  ssa
    putStrLn $ PP.ppShow hast

    putStrLn "\n-------- HSC --------" 
    hsc <- HSC.run  hast
    putStrLn $ hsc

    return ()



