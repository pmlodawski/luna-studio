---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

module Luna.Pass.Analysis.ID.ExtractIDs where

import           Control.Monad.State
import           Data.IntSet                 (IntSet)

import           Flowbox.Prelude             hiding (mapM, mapM_)
import           Luna.Pass                   (Pass (Pass))
import           Luna.Pass.Analysis.ID.State (IDState)
import qualified Luna.Pass.Analysis.ID.State as State
import           Luna.Syntax.Enum            (Enumerated)
import qualified Luna.Syntax.Enum            as Enum
import           Luna.Syntax.Expr            (LExpr)
import           Luna.Syntax.Graph.Node.Expr (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr as NodeExpr
import           Luna.Syntax.Label           (Label)
import qualified Luna.Syntax.Label           as Label
import qualified Luna.Syntax.Traversals      as AST



----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data ExtractIDs = ExtractIDs

type EIDPass                 m   = StateT IDState m
type EIDCtx              lab m a = (Enumerated lab, EIDTraversal m a)
type EIDTraversal            m a = (Monad m, Functor m, AST.Traversal        ExtractIDs (EIDPass m) a a)
type EIDDefaultTraversal     m a = (Monad m, Functor m, AST.DefaultTraversal ExtractIDs (EIDPass m) a a)

----------------------------------------------------------------------
-- Utils functions
----------------------------------------------------------------------

traverseM :: EIDTraversal m a => a -> EIDPass m a
traverseM = AST.traverseM ExtractIDs

defaultTraverseM :: EIDDefaultTraversal m a => a -> EIDPass m a
defaultTraverseM = AST.defaultTraverseM ExtractIDs

----------------------------------------------------------------------
-- Pass functions
----------------------------------------------------------------------

pass :: EIDDefaultTraversal m a => Pass IDState (a -> EIDPass m IntSet)
pass = Pass "Extract IDs"
            "Extract all IDs contained in labels"
            def extract

extract :: EIDDefaultTraversal m a => a -> EIDPass m IntSet
extract ast = defaultTraverseM ast >> State.getIDs


extractNodeExpr :: (Enumerated a, EIDDefaultTraversal m (LExpr a v))
                => NodeExpr a v -> EIDPass m IntSet
extractNodeExpr (NodeExpr.ASTExpr lexpr) = extract lexpr
extractNodeExpr _                        = return def


extractNodeExprs :: (Enumerated a, EIDDefaultTraversal m (LExpr a v))
                 => [NodeExpr a v] -> EIDPass m IntSet
extractNodeExprs []    = State.getIDs
extractNodeExprs (h:t) = extractNodeExpr h >> extractNodeExprs t


run :: EIDDefaultTraversal Identity a => a -> IntSet
run a = evalState (extract a) def


runNodeExpr :: (Enumerated a, EIDDefaultTraversal Identity (LExpr a v))
            => NodeExpr a v -> IntSet
runNodeExpr a = evalState (extractNodeExpr a) def


runNodeExprs :: (Enumerated a, EIDDefaultTraversal Identity (LExpr a v))
             => [NodeExpr a v] -> IntSet
runNodeExprs a = evalState (extractNodeExprs a) def


eidLabel :: EIDCtx lab m a => Label lab a -> EIDPass m (Label lab a)
eidLabel label = do
    State.appendID $ Enum.id $ label ^. Label.label
    defaultTraverseM label


instance EIDCtx lab m a => AST.Traversal ExtractIDs (EIDPass m) (Label lab a) (Label lab a) where
    traverseM _ = eidLabel
