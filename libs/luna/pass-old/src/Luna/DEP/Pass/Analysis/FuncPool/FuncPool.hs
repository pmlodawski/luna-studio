---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.DEP.Pass.Analysis.FuncPool.FuncPool where

import           Control.Applicative
import           Control.Monad.State                  hiding (mapM, mapM_)

import           Flowbox.Prelude                      hiding (error, id, mod)
import           Flowbox.System.Log.Logger
import qualified Luna.DEP.AST.Expr                    as Expr
import           Luna.DEP.AST.Module                  (Module)
import qualified Luna.DEP.AST.Module                  as Module
import           Luna.DEP.AST.Pat                     (Pat)
import qualified Luna.DEP.AST.Pat                     as Pat
import           Luna.DEP.AST.Type                    (Type)
import qualified Luna.DEP.AST.Type                    as Type
import           Luna.DEP.Pass.Analysis.FuncPool.Pool (Pool)
import qualified Luna.DEP.Pass.Analysis.FuncPool.Pool as Pool
import           Luna.DEP.Pass.Pass                   (Pass)
import qualified Luna.DEP.Pass.Pass                   as Pass



logger :: Logger
logger = getLogger $moduleName


type FPPass result = Pass Pool result


run :: Module -> Pass.Result Pool
run = (Pass.run_ (Pass.Info "FuncPool") Pool.empty) . fpMod


fpMod :: Module -> FPPass Pool
fpMod mod = do
    Module.traverseM_ fpMod fpExpr fpType fpPat pure pure mod
    get


fpExpr :: Expr.Expr -> FPPass ()
fpExpr ast = case ast of
    Expr.Function {}                      -> register
    Expr.Var      {}                      -> register
    Expr.Field    {}                      -> register
    _                                     -> continue
    where
        register  = Pool.register (ast ^. Expr.name) *> continue
        continue  = Expr.traverseM_ fpExpr fpType fpPat pure pure ast


fpPat :: Pat -> FPPass ()
fpPat pat = case pat of
    _                                   -> Pat.traverseM_ fpPat fpType pure pat

fpType :: Type -> FPPass ()
fpType t = case t of
    _                                   -> Type.traverseM_ fpType t
