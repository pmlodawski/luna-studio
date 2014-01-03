---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}

module Flowbox.Luna.Passes.Transform.SSA.SSA where

import qualified Flowbox.Luna.Data.AST.Module    as Module
import           Flowbox.Luna.Data.AST.Module      (Module)
import qualified Flowbox.Luna.Data.AST.Expr      as Expr
import qualified Flowbox.Luna.Data.AST.Pat       as Pat
import           Flowbox.Luna.Data.AST.Pat         (Pat)
import qualified Flowbox.Luna.Data.AliasAnalysis as AA
import           Flowbox.Luna.Data.AliasAnalysis   (AA)
import qualified Flowbox.Luna.Passes.Pass        as Pass
import           Flowbox.Luna.Passes.Pass          (Pass)
import qualified Data.IntMap                     as IntMap

import           Control.Monad.State               
import           Control.Applicative               

import           Flowbox.System.Log.Logger         

import           Flowbox.Prelude                 hiding (error, id, mod)


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.SSA.SSA"


type SSAPass result = Pass Pass.NoState result


mkVar :: Int -> String
mkVar id = "v_" ++ show id


run :: AA -> Module -> Pass.Result Module
run vs = (Pass.run_ (Pass.Info "SSA") Pass.NoState) . (ssaModule vs)
--run vs = (Pass.run_ (Pass.Info "SSA") Pass.NoState) . (ssaModule vs)


ssaModule :: AA -> Module -> SSAPass Module
ssaModule vs mod = Module.traverseM (ssaModule vs) (ssaExpr vs) pure ssaPat pure mod


ssaExpr :: AA -> Expr.Expr -> SSAPass Expr.Expr
ssaExpr vs ast = case ast of
    Expr.Accessor   id name dst           -> Expr.Accessor id name <$> ssaExpr vs dst
    Expr.Var        id name               -> case IntMap.lookup id (AA.varmap vs) of
                                                  Just nid -> return $ Expr.Var id (mkVar nid)
                                                  Nothing  -> (logger error $ "Not in scope '" ++ name ++ "'.")
                                                           *> (return $ Expr.Var id name)
    Expr.NativeVar  id name               -> case IntMap.lookup id (AA.varmap vs) of
                                                  Just nid -> return $ Expr.NativeVar id (mkVar nid)
                                                  Nothing  -> Pass.fail ("Not in scope '" ++ name ++ "'.")
    _                                     -> Expr.traverseM (ssaExpr vs) pure ssaPat pure ast


ssaPat :: Pat -> SSAPass Pat
ssaPat pat = case pat of
    Pat.Var  id _  -> return $ Pat.Var id (mkVar id)
    _              -> Pat.traverseM ssaPat pure pure pat