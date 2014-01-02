---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Analysis.ID.MaxID where

import           Flowbox.Luna.Data.AST.Expr               (Expr)
import           Flowbox.Luna.Data.AST.Module             (Module)
import qualified Flowbox.Luna.Data.AST.Utils              as AST
import           Flowbox.Luna.Passes.Analysis.ID.State    (IDState)
import qualified Flowbox.Luna.Passes.Analysis.ID.State    as State
import qualified Flowbox.Luna.Passes.Analysis.ID.Traverse as IDTraverse
import           Flowbox.Luna.Passes.Pass                 (PassMonad)
import qualified Flowbox.Luna.Passes.Pass                 as Pass
import           Flowbox.Prelude                          hiding (mapM, mapM_)
import           Flowbox.System.Log.Logger

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Analysis.ID.MaxID"


type MaxIDMonad m = PassMonad IDState m


run :: PassMonad s m => Module -> Pass.Result m AST.ID
run = (Pass.run_ (Pass.Info "MaxID") $ State.make) . analyseModule


runExpr :: PassMonad s m => Expr -> Pass.Result m AST.ID
runExpr = (Pass.run_ (Pass.Info "MaxID") $ State.make) . analyseExpr


analyseModule :: MaxIDMonad m => Module -> Pass.Result m AST.ID
analyseModule m = do IDTraverse.traverseModule State.compareID m
                     State.getMaxID

analyseExpr :: MaxIDMonad m => Expr -> Pass.Result m AST.ID
analyseExpr e = do IDTraverse.traverseExpr State.compareID e
                   State.getMaxID
