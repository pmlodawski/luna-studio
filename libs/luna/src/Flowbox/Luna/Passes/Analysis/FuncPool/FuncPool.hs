---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool where

import qualified Flowbox.Luna.Data.AST.Expr                  as Expr
import qualified Flowbox.Luna.Data.AST.Type                  as Type
import           Flowbox.Luna.Data.AST.Type                    (Type)
import qualified Flowbox.Luna.Data.AST.Pat                   as Pat
import           Flowbox.Luna.Data.AST.Pat                     (Pat)
import qualified Flowbox.Luna.Data.AST.Module                as Module
import           Flowbox.Luna.Data.AST.Module                  (Module)
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.State as Pool
import           Flowbox.Luna.Passes.Analysis.FuncPool.State   (Pool)
import qualified Flowbox.Luna.Passes.Pass                    as Pass
import           Flowbox.Luna.Passes.Pass                      (PassMonad)

import           Control.Monad.State                         hiding (mapM, mapM_)
import           Control.Applicative                           

import           Flowbox.System.Log.Logger                     

import           Flowbox.Prelude                             hiding (error, id, mod)


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.FuncPool.FuncPool"


type FPMonad m = PassMonad Pool m


run :: PassMonad s m => Module -> Pass.Result m Pool
run = (Pass.run_ (Pass.Info "FuncPool") Pool.empty) . fpMod


fpMod :: FPMonad m => Module -> Pass.Result m Pool
fpMod mod = do
    Module.traverseM_ fpMod fpExpr fpType fpPat pure mod
    get


fpExpr :: FPMonad m => Expr.Expr -> Pass.Result m ()
fpExpr ast = case ast of
    Expr.Function {}                      -> register
    Expr.Var      {}                      -> register
    _                                     -> continue
    where
        register  = Pool.register (Expr.name ast) *> continue
        continue  = Expr.traverseM_ fpExpr fpType fpPat pure ast


fpPat :: FPMonad m => Pat -> Pass.Result m ()
fpPat pat = case pat of
    _                                   -> Pat.traverseM_ fpPat fpType pure pat

fpType :: FPMonad m => Type -> Pass.Result m ()
fpType t = case t of
    _                                   -> Type.traverseM_ fpType t
