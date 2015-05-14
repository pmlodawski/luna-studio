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

module Luna.DEP.Pass.Analysis.ID.MaxID where

import           Flowbox.Prelude                    hiding (mapM, mapM_)
import           Flowbox.System.Log.Logger
import qualified Luna.DEP.AST.Common                as AST
import           Luna.DEP.AST.Expr                  (Expr)
import           Luna.DEP.AST.Module                (Module)
import           Luna.DEP.Pass.Analysis.ID.State    (IDState)
import qualified Luna.DEP.Pass.Analysis.ID.State    as State
import qualified Luna.DEP.Pass.Analysis.ID.Traverse as IDTraverse
import           Luna.DEP.Pass.Pass                 (Pass)
import qualified Luna.DEP.Pass.Pass                 as Pass



logger :: Logger
logger = getLogger $moduleName


type MaxIDPass result = Pass IDState result


run :: Module -> Pass.Result AST.ID
run = Pass.run_ (Pass.Info "MaxID") State.make . analyseModule


runExpr :: Expr -> Pass.Result AST.ID
runExpr = Pass.run_ (Pass.Info "MaxID") State.make . analyseExpr


analyseModule :: Module -> MaxIDPass AST.ID
analyseModule m = do IDTraverse.traverseModule State.findMaxID m
                     State.getFoundID


analyseExpr :: Expr -> MaxIDPass AST.ID
analyseExpr e = do IDTraverse.traverseExpr State.findMaxID e
                   State.getFoundID
