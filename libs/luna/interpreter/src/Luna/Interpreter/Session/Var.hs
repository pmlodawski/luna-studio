---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Luna.Interpreter.Session.Var where

import Control.Monad.State

import           Flowbox.Prelude
import qualified Luna.AST.Common                  as AST
import           Luna.AST.Expr                    (Expr)
import qualified Luna.AST.Expr                    as Expr
import qualified Luna.AST.Lit                     as Lit
import qualified Luna.AST.Lit.Number              as Number
import           Luna.Graph.Node.Expr             (NodeExpr)
import qualified Luna.Graph.Node.Expr             as NodeExpr
import qualified Luna.Interpreter.Session.Env     as Env
import           Luna.Interpreter.Session.Session (Session)



timeRef :: String
timeRef = "time"


timeRefIds :: NodeExpr -> [AST.ID]
timeRefIds (NodeExpr.ASTExpr expr) = execState (traverseExpr expr) [] where
    traverseExpr = Expr.traverseMR matchTimeRef return return return return
    matchTimeRef e = case matchesTimeRef e of
        Just i  -> modify (i:) >> return e
        Nothing -> return e
timeRefIds  _                      = []


replaceTimeRefs :: Expr -> Session mm Expr
replaceTimeRefs = Expr.traverseMR replace return return return return where
    replace e = case matchesTimeRef e of
        Nothing -> return e
        Just i  -> do
            (int :: Int, frac) <- properFraction <$> Env.getTimeVar
            return $ Expr.Lit i $ Lit.Number i $ Number.decimal (Number.Float (show int) (tail $ tail $ show frac)) Nothing Number.Positive


matchesTimeRef :: Expr -> Maybe AST.ID
matchesTimeRef (Expr.App _ (Expr.Accessor i (Expr.ConAccessor "time") (Expr.App _ (Expr.Con _ "Std") [])) []) = Just i
matchesTimeRef _ = Nothing
