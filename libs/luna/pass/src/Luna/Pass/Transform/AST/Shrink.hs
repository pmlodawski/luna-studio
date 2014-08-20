---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Pass.Transform.AST.Shrink (
    shrinkFunctionBodies,
) where

import           Luna.AST.Expr         (Expr)
import qualified Luna.AST.Expr         as Expr
import           Luna.AST.Module       (Module)
import qualified Luna.AST.Module       as Module
import           Luna.AST.Control.Focus (Focus)
import qualified Luna.AST.Control.Focus as Focus
import           Flowbox.Prelude



shrinkFunctionBodies :: (Applicative m, Monad m) => Focus -> m Focus
shrinkFunctionBodies = Focus.traverseM shrinkModule shrinkExpr


shrinkModule :: (Applicative m, Monad m) => Module -> m Module
shrinkModule m = Module.traverseM shrinkModule shrinkExpr pure pure pure m


shrinkExpr :: (Applicative m, Monad m) => Expr -> m Expr
shrinkExpr e = case e of
     Expr.Function i path name inputs output _ -> pure $ Expr.Function i path name inputs output []
     _                                         -> Expr.traverseM shrinkExpr pure pure pure e

