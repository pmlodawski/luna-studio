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

module Flowbox.Luna.Passes.Transform.AST.Desugar.Desugar where

import           Flowbox.Luna.Data.Analysis.Alias.Alias  (AA (AA))
import qualified Flowbox.Luna.Data.AST.Expr              as Expr
import           Flowbox.Luna.Data.AST.Module            (Module)
import qualified Flowbox.Luna.Data.AST.Module            as Module
import           Flowbox.Luna.Data.AST.Pat               (Pat)
import qualified Flowbox.Luna.Data.AST.Pat               as Pat
import           Flowbox.Luna.Passes.Pass                (Pass)
import qualified Flowbox.Luna.Passes.Pass                as Pass
import           Flowbox.Prelude                         hiding (error, id, mod)
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.AST.Desugar.Desugar"


type DesugarPass result = Pass Pass.NoState result


run :: AA -> Module -> Pass.Result Module
run va = (Pass.run_ (Pass.Info "Desugar") Pass.NoState) . desugarModule


desugarModule :: Module -> DesugarPass Module
desugarModule mod = Module.traverseM desugarModule desugarExpr pure desugarPat pure mod


desugarExpr :: Expr.Expr -> DesugarPass Expr.Expr
desugarExpr ast = case ast of
    Expr.Con {}                           -> Expr.App 0 <$> continue <*> pure [] 
    Expr.App {}                           -> omitNext
    _                                     -> continue
    where continue = Expr.traverseM desugarExpr pure desugarPat pure ast
          omitNext = Expr.traverseM omitExpr pure desugarPat pure ast

omitExpr :: Expr.Expr -> DesugarPass Expr.Expr
omitExpr ast = continue
    where continue = Expr.traverseM desugarExpr pure desugarPat pure ast


desugarPat :: Pat -> DesugarPass Pat
desugarPat pat = case pat of
    _              -> continue
    where continue = Pat.traverseM desugarPat pure pure pat
