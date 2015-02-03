---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Var where

import Control.Monad.State

import           Flowbox.Prelude
import qualified Luna.DEP.AST.Common      as AST
import qualified Luna.DEP.AST.Expr        as Expr
import           Luna.Graph.Node.Expr (NodeExpr)
import qualified Luna.Graph.Node.Expr as NodeExpr



timeRef :: String
timeRef = "time"


timeRefIds :: NodeExpr -> [AST.ID]
timeRefIds (NodeExpr.ASTExpr expr) = execState (traverseExpr expr) [] where
    traverseExpr = Expr.traverseMR matchTimeRef return return return return
    matchTimeRef e@(Expr.App _ (Expr.Accessor i (Expr.ConAccessor "time") (Expr.App _ (Expr.Con _ "Std") [])) [])
                   = modify (i:) >> return e
    matchTimeRef e = return e
timeRefIds  _                      = []

