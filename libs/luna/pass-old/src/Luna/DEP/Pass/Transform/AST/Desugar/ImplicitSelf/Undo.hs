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

module Luna.DEP.Pass.Transform.AST.Desugar.ImplicitSelf.Undo where

import           Flowbox.Prelude                                   hiding (error, id, mod)
import           Flowbox.System.Log.Logger
import           Luna.DEP.AST.Expr                                 (Expr)
import qualified Luna.DEP.AST.Expr                                 as Expr
import           Luna.DEP.AST.Module                               (Module)
import qualified Luna.DEP.AST.Module                               as Module
import qualified Luna.DEP.AST.Pat                                  as Pat
import           Luna.DEP.Data.ASTInfo                             (ASTInfo)
import           Luna.DEP.Pass.Pass                                (Pass)
import qualified Luna.DEP.Pass.Pass                                as Pass
import           Luna.DEP.Pass.Transform.AST.Desugar.General.State (DesugarState)
import qualified Luna.DEP.Pass.Transform.AST.Desugar.General.State as State



logger :: LoggerIO
logger = getLoggerIO $moduleName


type DesugarPass result = Pass DesugarState result


run :: ASTInfo -> Module -> Pass.Result (Module, ASTInfo)
run inf = Pass.run_ (Pass.Info "Desugar.ImplicitSelf.Undo") (State.mk inf) . process


runExpr :: ASTInfo -> Expr -> Pass.Result (Expr, ASTInfo)
runExpr inf = Pass.run_ (Pass.Info "Desugar.ImplicitSelf.Undo") (State.mk inf) . process'


process :: Module -> DesugarPass (Module, ASTInfo)
process mod = (,) <$> processModule mod <*> State.getInfo


process' :: Expr -> DesugarPass (Expr, ASTInfo)
process' expr = (,) <$> processExpr expr <*> State.getInfo


processModule :: Module -> DesugarPass Module
processModule = Module.traverseM processModule processExpr pure pure pure pure


processExpr :: Expr.Expr -> DesugarPass Expr.Expr
processExpr ast = case ast of
    Expr.Function {} -> return $ ast & Expr.inputs %~ deleteSelf
                        where deleteSelf [] = []
                              deleteSelf (Expr.Arg _ (Pat.Var _ "self") Nothing : t) = t
                              deleteSelf other = other
    _                -> continue
    where continue  = Expr.traverseM processExpr pure pure pure pure ast


