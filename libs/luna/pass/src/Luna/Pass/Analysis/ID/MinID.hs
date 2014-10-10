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

module Luna.Pass.Analysis.ID.MinID where

import           Flowbox.Prelude                hiding (mapM, mapM_)
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Common                as AST
import           Luna.AST.Expr                  (Expr)
import           Luna.AST.Module                (Module)
import           Luna.Pass.Analysis.ID.State    (IDState)
import qualified Luna.Pass.Analysis.ID.State    as State
import qualified Luna.Pass.Analysis.ID.Traverse as IDTraverse
import           Luna.Pass.Pass                 (Pass)
import qualified Luna.Pass.Pass                 as Pass



logger :: Logger
logger = getLogger $(moduleName)


type MinIDPass result = Pass IDState result


run :: Module -> Pass.Result AST.ID
run = (Pass.run_ (Pass.Info "MinID") State.make) . analyseModule


runExpr :: Expr -> Pass.Result AST.ID
runExpr = (Pass.run_ (Pass.Info "MinID") State.make) . analyseExpr


analyseModule :: Module -> MinIDPass AST.ID
analyseModule m = do IDTraverse.traverseModule State.findMinID m
                     State.getFoundID


analyseExpr :: Expr -> MinIDPass AST.ID
analyseExpr e = do IDTraverse.traverseExpr State.findMinID e
                   State.getFoundID
