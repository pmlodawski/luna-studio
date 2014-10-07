---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}

module Luna.Pass.Analysis.ID.ExtractIDs where

import Data.IntSet (IntSet)

import           Flowbox.Prelude                hiding (mapM, mapM_)
import           Flowbox.System.Log.Logger
import           Luna.AST.Control.Focus         (Focus)
import           Luna.AST.Expr                  (Expr)
import           Luna.Pass.Analysis.ID.State    (IDState)
import qualified Luna.Pass.Analysis.ID.State    as State
import qualified Luna.Pass.Analysis.ID.Traverse as IDTraverse
import           Luna.Pass.Pass                 (Pass)
import qualified Luna.Pass.Pass                 as Pass



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Analysis.ID.ExtractIDs"


type ExtractIDPass result = Pass IDState result


run :: Focus -> Pass.Result IntSet
run = Pass.run_ (Pass.Info "ExtractIDs") State.make . analyseFocus


runExpr :: Expr -> Pass.Result IntSet
runExpr = Pass.run_ (Pass.Info "ExtractIDs") State.make . analyseExpr


analyseFocus :: Focus -> ExtractIDPass IntSet
analyseFocus m = IDTraverse.traverseFocus State.appendID m >> State.getIDs

analyseExpr :: Expr -> ExtractIDPass IntSet
analyseExpr e = IDTraverse.traverseExpr State.appendID e >> State.getIDs
