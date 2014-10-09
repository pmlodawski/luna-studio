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

module Luna.Pass.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt where

import           Flowbox.Prelude                               hiding (error, id, mod)
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Expr                                 as Expr
import           Luna.AST.Module                               (Module)
import qualified Luna.AST.Module                               as Module
import           Luna.AST.Pat                                  (Pat)
import qualified Luna.AST.Pat                                  as Pat
import           Luna.Data.ASTInfo                             (ASTInfo)
import           Luna.Pass.Pass                                (Pass)
import qualified Luna.Pass.Pass                                as Pass
import           Luna.Pass.Transform.AST.Desugar.General.State (DesugarState)
import qualified Luna.Pass.Transform.AST.Desugar.General.State as DS


logger :: LoggerIO
logger = getLoggerIO $(moduleName)


type DesugarPass result = Pass DesugarState result


run :: ASTInfo -> Module -> Pass.Result (Module, ASTInfo)
run inf = (Pass.run_ (Pass.Info "Desugar.TLRecUpdt") $ DS.mk inf) . desugar


desugar :: Module -> DesugarPass (Module, ASTInfo)
desugar mod = (,) <$> desugarModule mod <*> DS.getInfo


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
          --omitNext  = Expr.traverseM omitExpr pure desugarPat pure ast

desugarFuncTLExpr :: Expr.Expr -> DesugarPass Expr.Expr
desugarFuncTLExpr ast = case ast of
    Expr.RecordUpdate _id src _selectors _expr -> case src of
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
