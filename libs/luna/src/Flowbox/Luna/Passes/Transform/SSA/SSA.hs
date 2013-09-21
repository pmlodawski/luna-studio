---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Transform.SSA.SSA where

import qualified Flowbox.Luna.Data.AST.Module    as Module
import           Flowbox.Luna.Data.AST.Module      (Module)
import qualified Flowbox.Luna.Data.AST.Expr      as Expr
import qualified Flowbox.Luna.Data.AST.Type      as Type
import           Flowbox.Luna.Data.AST.Type        (Type)
import qualified Flowbox.Luna.Data.AST.Pat       as Pat
import           Flowbox.Luna.Data.AST.Pat         (Pat)
import qualified Flowbox.Luna.Data.AliasAnalysis as AA
import           Flowbox.Luna.Data.AliasAnalysis   (AA)
import qualified Flowbox.Luna.Passes.Pass        as Pass
import           Flowbox.Luna.Passes.Pass          (PassMonad)
import qualified Data.IntMap                     as IntMap

import           Control.Monad.State               
import           Control.Applicative               

import           Flowbox.System.Log.Logger         

import           Flowbox.Prelude                 hiding (error, id)



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.SSA.SSA"


type SSAMonad m = PassMonad Pass.NoState m


mkVar id = "v_" ++ show id


run :: PassMonad s m => AA -> Module -> Pass.Result m Module
run vs = (Pass.run_ Pass.NoState) . (ssaModule vs)


ssaModule :: SSAMonad m => AA -> Module -> Pass.Result m Module
ssaModule vs mod = Module.traverseM (ssaExpr vs) pure ssaPat pure mod


ssaExpr :: SSAMonad m => AA -> Expr.Expr -> Pass.Result m Expr.Expr
ssaExpr vs ast = case ast of
    Expr.Accessor   id src dst            -> Expr.Accessor id <$> ssaExpr vs src <*> pure dst
    Expr.Var        id name               -> case IntMap.lookup id (AA.varmap vs) of
                                                  Just nid -> return $ Expr.Var id (mkVar nid)
                                                  Nothing  -> logger error ("Not in scope '" ++ name ++ "'") *> Pass.fail ("Not in scope '" ++ name ++ "'")
    _                                     -> Expr.traverseM (ssaExpr vs) pure ssaPat pure ast


ssaPat :: SSAMonad m => Pat -> Pass.Result m Pat
ssaPat pat = case pat of
    Pat.Var  id name  -> return $ Pat.Var id (mkVar id)
    _                 -> Pat.traverseM ssaPat pure pure pat