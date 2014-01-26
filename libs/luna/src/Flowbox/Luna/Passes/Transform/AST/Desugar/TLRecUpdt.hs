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

module Flowbox.Luna.Passes.Transform.AST.Desugar.TLRecUpdt where

import           Flowbox.Luna.Data.Analysis.Alias.Alias          (AA (AA))
import qualified Flowbox.Luna.Data.AST.Expr                      as Expr
import           Flowbox.Luna.Data.AST.Module                    (Module)
import qualified Flowbox.Luna.Data.AST.Module                    as Module
import           Flowbox.Luna.Data.AST.Pat                       (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                       as Pat
import           Flowbox.Luna.Data.Pass.ASTInfo                  (ASTInfo)
import qualified Flowbox.Luna.Data.Pass.ASTInfo                  as ASTInfo
import           Flowbox.Luna.Passes.Pass                        (Pass)
import qualified Flowbox.Luna.Passes.Pass                        as Pass
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.State as DesugarState
import           Flowbox.Luna.Passes.Transform.AST.Desugar.State (DesugarState)
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.State as DS
import           Flowbox.Prelude                                 hiding (error, id, mod)
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.AST.Desugar.TLRecUpdt"


type DesugarPass result = Pass DesugarState result


run :: ASTInfo -> Module -> Pass.Result (Module, ASTInfo)
run info = (Pass.run_ (Pass.Info "Desugar.TLRecUpdt") $ DS.mk info) . desugar


desugar :: Module -> DesugarPass (Module, ASTInfo)
desugar mod = (,) <$> desugarModule mod <*> DesugarState.getInfo


desugarModule :: Module -> DesugarPass Module
desugarModule mod = Module.traverseM desugarModule desugarExpr pure desugarPat pure mod


desugarExpr :: Expr.Expr -> DesugarPass Expr.Expr
desugarExpr ast = case ast of
    --Expr.Con {}                           -> Expr.App <$> DS.genID <*> continue <*> pure []
    --Expr.App {}                           -> omitNext
    Expr.Function id path name inputs
                  output body             -> Expr.Function id path name <$> fexpMap inputs <*> ftype output <*> fexpTLMap body
    _                                     -> continue
    where ftype     = pure
          fexpMap   = mapM desugarExpr
          fexpTLMap = mapM desugarFuncTLExpr
          continue  = Expr.traverseM desugarExpr pure desugarPat pure ast
          omitNext  = Expr.traverseM omitExpr pure desugarPat pure ast

desugarFuncTLExpr :: Expr.Expr -> DesugarPass Expr.Expr
desugarFuncTLExpr ast = case ast of
    Expr.RecordUpdate id src selectors expr -> case src of
                                                   Expr.Var _ name -> Expr.Assignment <$> DS.genID <*> (Pat.Var <$> DS.genID <*> pure name) <*> pure ast
                                                   _               -> desugarExpr ast
    _                                       -> desugarExpr ast

omitExpr :: Expr.Expr -> DesugarPass Expr.Expr
omitExpr ast = continue
    where continue = Expr.traverseM desugarExpr pure desugarPat pure ast


desugarPat :: Pat -> DesugarPass Pat
desugarPat pat = case pat of
    _              -> continue
    where continue = Pat.traverseM desugarPat pure pure pat
