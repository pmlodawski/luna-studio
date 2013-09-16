---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.SSA.SSA where

import qualified Flowbox.Luna.AST.Expr        as Expr
import qualified Flowbox.Luna.AST.Type        as Type
import           Flowbox.Luna.AST.Type          (Type)
import qualified Flowbox.Luna.AST.Pat         as Pat
import           Flowbox.Luna.AST.Pat           (Pat)
import           Flowbox.Luna.Passes.VA.State   (VarStat)
import qualified Flowbox.Luna.Passes.VA.State as VarStat
import qualified Flowbox.Luna.Passes.Pass     as Pass
import           Flowbox.Luna.Passes.Pass       (PassMonad)
import qualified Data.IntMap                  as IntMap

import           Control.Monad.State            
import           Control.Applicative            

import           Flowbox.System.Log.Logger      

import           Flowbox.Prelude              hiding (error, id)



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.SSA.SSA"


type SSAMonad m = PassMonad Pass.NoState m


mkVar id = "v_" ++ show id

run :: PassMonad s m => VarStat -> Expr.Expr -> Pass.Result m Expr.Expr
run vs = (Pass.run_ Pass.NoState) . (ssaExpr vs)


ssaExpr :: SSAMonad m => VarStat -> Expr.Expr -> Pass.Result m Expr.Expr
ssaExpr vs ast = case ast of
    Expr.Var        id name               -> case IntMap.lookup id (VarStat.varmap vs) of
                                                  Just nid -> return $ Expr.Var id (mkVar nid)
                                                  Nothing  -> logger error ("not in scope '" ++ name ++ "'") *> Pass.fail ("not in scope '" ++ name ++ "'")
    _                                     -> Expr.traverseM (ssaExpr vs) pure ssaPat pure ast


ssaPat :: SSAMonad m => Pat -> Pass.Result m Pat
ssaPat pat = case pat of
    Pat.Var  id name  -> return $ Pat.Var id (mkVar id)
    _                 -> Pat.traverseM ssaPat pure pure pat