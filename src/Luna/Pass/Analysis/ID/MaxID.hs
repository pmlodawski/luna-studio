---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}

module Luna.Pass.Analysis.ID.MaxID where

import qualified Flowbox.Luna.Data.AST.Common             as AST
import           Flowbox.Luna.Data.AST.Expr               (Expr)
import           Flowbox.Luna.Data.AST.Module             (Module)
import           Flowbox.Luna.Passes.Analysis.ID.State    (IDState)
import qualified Flowbox.Luna.Passes.Analysis.ID.State    as State
import qualified Flowbox.Luna.Passes.Analysis.ID.Traverse as IDTraverse
import           Flowbox.Luna.Passes.Pass                 (Pass)
import qualified Flowbox.Luna.Passes.Pass                 as Pass
import           Flowbox.Prelude                          hiding (mapM, mapM_)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Analysis.ID.MaxID"


type MaxIDPass result = Pass IDState result


run :: Module -> Pass.Result AST.ID
run = (Pass.run_ (Pass.Info "MaxID") $ State.make) . analyseModule


runExpr :: Expr -> Pass.Result AST.ID
runExpr = (Pass.run_ (Pass.Info "MaxID") $ State.make) . analyseExpr


analyseModule :: Module -> MaxIDPass AST.ID
analyseModule m = do IDTraverse.traverseModule State.compareID m
                     State.getMaxID


analyseExpr :: Expr -> MaxIDPass AST.ID
analyseExpr e = do IDTraverse.traverseExpr State.compareID e
                   State.getMaxID
