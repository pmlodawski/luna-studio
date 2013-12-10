---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Transform.AST.Shrink (
    shrinkFunctionBodies,
) where

import           Flowbox.Luna.Data.AST.Expr   (Expr)
import qualified Flowbox.Luna.Data.AST.Expr   as Expr
import           Flowbox.Luna.Data.AST.Module (Module)
import qualified Flowbox.Luna.Data.AST.Module as Module
import           Flowbox.Prelude



shrinkFunctionBodies :: (Applicative m, Monad m) => Module -> m Module
shrinkFunctionBodies m = Module.traverseM pure shrinkExpr pure pure pure m


shrinkExpr :: (Applicative m, Monad m) => Expr -> m Expr
shrinkExpr e = case e of
     Expr.Function i path name inputs output _ -> pure $ Expr.Function i path name inputs output []
     _                                         -> Expr.traverseM shrinkExpr pure pure pure e

