---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.DEP.Pass.Analysis.ID.ExtractIDs where

import           Data.IntSet                        (IntSet)

import           Flowbox.Prelude                    hiding (mapM, mapM_)
import           Flowbox.System.Log.Logger
import           Luna.DEP.AST.Control.Focus         (Focus)
import           Luna.DEP.AST.Expr                  (Expr)
import           Luna.DEP.AST.Module                (Module)
import           Luna.DEP.AST.Pat                   (Pat)
import           Luna.DEP.Graph.Node.Expr           (NodeExpr)
import qualified Luna.DEP.Graph.Node.Expr           as NodeExpr
import           Luna.DEP.Pass.Analysis.ID.State    (IDState)
import qualified Luna.DEP.Pass.Analysis.ID.State    as State
import qualified Luna.DEP.Pass.Analysis.ID.Traverse as IDTraverse
import           Luna.DEP.Pass.Pass                 (Pass)
import qualified Luna.DEP.Pass.Pass                 as Pass



logger :: Logger
logger = getLogger $moduleName


type ExtractIDPass result = Pass IDState result


runPass :: (Monad m, Functor m)
        => Pass.ESRT err Pass.Info IDState m result
        -> m (Either err result)
runPass = Pass.run_ (Pass.Info "ExtractIDs") State.make


run :: Focus -> Pass.Result IntSet
run = runPass . analyseFocus


runExpr :: Expr -> Pass.Result IntSet
runExpr = runPass . analyseExpr


runModule :: Module -> Pass.Result IntSet
runModule = runPass . analyseModule


runPat :: Pat -> Pass.Result IntSet
runPat = runPass . analysePat


runNodeExpr :: NodeExpr -> Pass.Result IntSet
runNodeExpr = runPass . analyseNodeExpr


runNodeExprs :: [NodeExpr] -> Pass.Result IntSet
runNodeExprs = runPass . analyseNodeExprs


analyseFocus :: Focus -> ExtractIDPass IntSet
analyseFocus m = IDTraverse.traverseFocus State.appendID m >> State.getIDs


analyseExpr :: Expr -> ExtractIDPass IntSet
analyseExpr e = IDTraverse.traverseExpr State.appendID e >> State.getIDs


analyseModule :: Module -> ExtractIDPass IntSet
analyseModule e = IDTraverse.traverseModule State.appendID e >> State.getIDs


analysePat :: Pat -> ExtractIDPass IntSet
analysePat p = IDTraverse.traversePat State.appendID p >> State.getIDs


analyseNodeExpr :: NodeExpr -> ExtractIDPass IntSet
analyseNodeExpr (NodeExpr.ASTExpr expr) = analyseExpr expr
analyseNodeExpr _                       = return def


analyseNodeExprs :: [NodeExpr] -> ExtractIDPass IntSet
analyseNodeExprs []                        = State.getIDs
analyseNodeExprs (NodeExpr.ASTExpr expr:t) = analyseExpr expr >> analyseNodeExprs t
analyseNodeExprs (_                    :t) = analyseNodeExprs t
