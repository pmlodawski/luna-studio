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

module Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool where

import Control.Applicative
import Control.Monad.State hiding (mapM, mapM_)

import qualified Flowbox.Luna.Data.AST.Expr                 as Expr
import           Flowbox.Luna.Data.AST.Module               (Module)
import qualified Flowbox.Luna.Data.AST.Module               as Module
import           Flowbox.Luna.Data.AST.Pat                  (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                  as Pat
import           Flowbox.Luna.Data.AST.Type                 (Type)
import qualified Flowbox.Luna.Data.AST.Type                 as Type
import           Flowbox.Luna.Passes.Analysis.FuncPool.Pool (Pool)
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.Pool as Pool
import           Flowbox.Luna.Passes.Pass                   (Pass)
import qualified Flowbox.Luna.Passes.Pass                   as Pass
import           Flowbox.Prelude                            hiding (error, id, mod)
import           Flowbox.System.Log.Logger

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.FuncPool.FuncPool"


type FPPass result = Pass Pool result


run :: Module -> Pass.Result Pool
run = (Pass.run_ (Pass.Info "FuncPool") Pool.empty) . fpMod


fpMod :: Module -> FPPass Pool
fpMod mod = do
    Module.traverseM_ fpMod fpExpr fpType fpPat pure mod
    get


fpExpr :: Expr.Expr -> FPPass ()
fpExpr ast = case ast of
    Expr.Function {}                      -> register
    Expr.Var      {}                      -> register
    Expr.Field    {}                      -> register
    _                                     -> continue
    where
        register  = Pool.register (ast ^. Expr.name) *> continue
        continue  = Expr.traverseM_ fpExpr fpType fpPat pure ast


fpPat :: Pat -> FPPass ()
fpPat pat = case pat of
    _                                   -> Pat.traverseM_ fpPat fpType pure pat

fpType :: Type -> FPPass ()
fpType t = case t of
    _                                   -> Type.traverseM_ fpType t
