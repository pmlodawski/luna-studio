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

module Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitScopes where

import qualified Flowbox.Luna.Data.AST.Expr                      as Expr
import           Flowbox.Luna.Data.AST.Module                    (Module)
import qualified Flowbox.Luna.Data.AST.Module                    as Module
import           Flowbox.Luna.Data.AST.Pat                       (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                       as Pat
import           Flowbox.Luna.Data.Pass.AliasInfo                (AliasInfo)
import           Flowbox.Luna.Data.Pass.ASTInfo                  (ASTInfo)
import           Flowbox.Luna.Passes.Pass                        (Pass)
import qualified Flowbox.Luna.Passes.Pass                        as Pass
import           Flowbox.Luna.Passes.Transform.AST.Desugar.State (DesugarState)
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.State as DesugarState
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.State as DS
import           Flowbox.Prelude                                 hiding (error, id, mod)
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.AST.Desugar.ImplicitScopes"


type DesugarPass result = Pass DesugarState result


run :: ASTInfo -> AliasInfo -> Module -> Pass.Result (Module, ASTInfo)
run inf va = (Pass.run_ (Pass.Info "Desugar.ImplicitScopes") $ DS.mk inf) . desugar


desugar :: Module -> DesugarPass (Module, ASTInfo)
desugar mod = (,) <$> desugarModule mod <*> DesugarState.getInfo


desugarModule :: Module -> DesugarPass Module
desugarModule mod = Module.traverseM desugarModule desugarExpr pure desugarPat pure mod


desugarExpr :: Expr.Expr -> DesugarPass Expr.Expr
desugarExpr ast = case ast of
    _                                     -> continue
    where continue  = Expr.traverseM desugarExpr pure desugarPat pure ast


desugarPat :: Pat -> DesugarPass Pat
desugarPat pat = case pat of
    _              -> continue
    where continue = Pat.traverseM desugarPat pure pure pat
