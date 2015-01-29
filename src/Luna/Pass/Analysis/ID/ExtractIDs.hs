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
{-# LANGUAGE TemplateHaskell       #-}

module Luna.Pass.Analysis.ID.ExtractIDs where

import Data.IntSet (IntSet)

import           Flowbox.Prelude                hiding (mapM, mapM_)
import           Flowbox.System.Log.Logger
import           Luna.Pass                      (Pass (Pass), PassCtx, PassMonad)
import qualified Luna.Pass                      as Pass
import           Luna.Pass.Analysis.ID.State    (IDState)
import qualified Luna.Pass.Analysis.ID.State    as State
import qualified Luna.Pass.Analysis.ID.Traverse as IDTraverse
import           Luna.Pass.Pass                 (Pass)
import qualified Luna.Pass.Pass                 as Pass
import           Luna.Syntax.Control.Focus      (Focus)
import           Luna.Syntax.Enum               (Enumerated)
import qualified Luna.Syntax.Enum               as Enum
import           Luna.Syntax.Expr               (Expr, LExpr)
import           Luna.Syntax.Graph.Node.Expr    (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr    as NodeExpr
import           Luna.Syntax.Label              (Label)
import qualified Luna.Syntax.Label              as Label
import           Luna.Syntax.Module             (Module)
import           Luna.Syntax.Pat                (Pat)
import qualified Luna.Syntax.Traversals         as AST



----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data ExtractIDs = ExtractIDs

type EIDPass                 m   = PassMonad IDState m
type EIDCtx              lab m a = (Enumerated lab, EIDTraversal m a)
type EIDTraversal            m a = (PassCtx m, AST.Traversal        ExtractIDs (EIDPass m) a a)
type EIDDefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal ExtractIDs (EIDPass m) a a)

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
            def run

run :: EIDDefaultTraversal m a => a -> EIDPass m IntSet
run ast = defaultTraverseM ast >> State.getIDs


eidLabel :: EIDCtx lab m a => Label lab a -> EIDPass m (Label lab a)
eidLabel label = do 
    State.appendID $ Enum.id $ label ^. Label.label
    defaultTraverseM label



runNodeExpr :: (Enumerated a, EIDDefaultTraversal m (LExpr a v)) => NodeExpr a v -> EIDPass m IntSet
runNodeExpr (NodeExpr.ASTExpr lexpr) = run lexpr
runNodeExpr _                        = return def


instance EIDCtx lab m a => AST.Traversal ExtractIDs (EIDPass m) (Label lab a) (Label lab a) where
    traverseM _ = eidLabel

--instance EIDCtx lab m a => AST.Traversal ExtractIDs (EIDPass m) (LDecl lab a) (LDecl lab a) where
--    traverseM _ = processDecl

--instance EIDCtx lab m v => AST.Traversal ExtractIDs (EIDPass m) (LExpr lab v) (LExpr lab v) where
--    traverseM _ = processExpr

--instance (PassCtx m, Enumerated lab) => AST.Traversal ExtractIDs (EIDPass m) (LPat lab) (LPat lab) where
--    traverseM _ = processPat

---------------------------


--type ExtractIDPass result = Pass IDState result


--runPass :: (Monad m, Functor m)
--        => Pass.ESRT err Pass.Info IDState m result
--        -> m (Either err result)
--runPass = Pass.run_ (Pass.Info "ExtractIDs") State.make


--run :: Focus -> Pass.Result IntSet
--run = runPass . analyseFocus


--runExpr :: Expr -> Pass.Result IntSet
--runExpr = runPass . analyseExpr


--runModule :: Module -> Pass.Result IntSet
--runModule = runPass . analyseModule


--runPat :: Pat -> Pass.Result IntSet
--runPat = runPass . analysePat


--runNodeExpr :: NodeExpr -> Pass.Result IntSet
--runNodeExpr = runPass . analyseNodeExpr


--runNodeExprs :: [NodeExpr] -> Pass.Result IntSet
--runNodeExprs = runPass . analyseNodeExprs


--analyseFocus :: Focus -> ExtractIDPass IntSet
--analyseFocus m = IDTraverse.traverseFocus State.appendID m >> State.getIDs


--analyseExpr :: Expr -> ExtractIDPass IntSet
--analyseExpr e = IDTraverse.traverseExpr State.appendID e >> State.getIDs




--analyseModule :: Module -> ExtractIDPass IntSet
--analyseModule e = IDTraverse.traverseModule State.appendID e >> State.getIDs


--analysePat :: Pat -> ExtractIDPass IntSet
--analysePat p = IDTraverse.traversePat State.appendID p >> State.getIDs



--analyseNodeExpr :: NodeExpr -> ExtractIDPass IntSet
--analyseNodeExpr (NodeExpr.ASTExpr expr) = analyseExpr expr
--analyseNodeExpr _                       = return def


--analyseNodeExprs :: [NodeExpr] -> ExtractIDPass IntSet
--analyseNodeExprs []                        = State.getIDs
--analyseNodeExprs (NodeExpr.ASTExpr expr:t) = analyseExpr expr >> analyseNodeExprs t
--analyseNodeExprs (_                    :t) = analyseNodeExprs t


