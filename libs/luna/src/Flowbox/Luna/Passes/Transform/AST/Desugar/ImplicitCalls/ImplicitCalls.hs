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

module Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls where

import qualified Flowbox.Luna.Data.AST.Expr                              as Expr
import           Flowbox.Luna.Data.AST.Module                            (Module)
import qualified Flowbox.Luna.Data.AST.Module                            as Module
import           Flowbox.Luna.Data.AST.Pat                               (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                               as Pat
import           Flowbox.Luna.Data.Pass.ASTInfo                          (ASTInfo)
import           Flowbox.Luna.Passes.Pass                                (Pass)
import qualified Flowbox.Luna.Passes.Pass                                as Pass
import           Flowbox.Luna.Passes.Transform.AST.Desugar.General.State (DesugarState)
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.General.State as DS
import           Flowbox.Prelude                                         hiding (error, id, mod)
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.AST.Desugar.ImplicitCalls.ImplicitCalls"


type DesugarPass result = Pass DesugarState result


run :: ASTInfo -> Module -> Pass.Result (Module, ASTInfo)
run inf = (Pass.run_ (Pass.Info "Desugar.ImplicitCalls") $ DS.mk inf) . desugar


desugar :: Module -> DesugarPass (Module, ASTInfo)
desugar mod = (,) <$> desugarModule mod <*> DS.getInfo


desugarModule :: Module -> DesugarPass Module
desugarModule mod = Module.traverseM desugarModule desugarExpr pure desugarPat pure mod


desugarExpr :: Expr.Expr -> DesugarPass Expr.Expr
desugarExpr ast = case ast of
    Expr.Con      {}                           -> Expr.App <$> DS.genID <*> continue <*> pure []
    Expr.App      id src args                  -> Expr.App id <$> omitNextExpr src <*> mapM desugarExpr args
    Expr.Accessor id name dst                  -> Expr.App <$> DS.genID <*> continue <*> pure []
    Expr.Import   {}                           -> omitAll
    _                                          -> continue
    where continue  = Expr.traverseM desugarExpr pure desugarPat pure ast
          omitNext  = Expr.traverseM omitNextExpr pure desugarPat pure ast
          omitAll   = Expr.traverseM omitAllExpr pure desugarPat pure ast


omitNextExpr :: Expr.Expr -> DesugarPass Expr.Expr
omitNextExpr ast = continue
    where continue = Expr.traverseM desugarExpr pure desugarPat pure ast

omitAllExpr :: Expr.Expr -> DesugarPass Expr.Expr
omitAllExpr ast = continue
    where continue = Expr.traverseM omitAllExpr pure desugarPat pure ast


desugarPat :: Pat -> DesugarPass Pat
desugarPat pat = case pat of
    _              -> continue
    where continue = Pat.traverseM desugarPat pure pure pat
