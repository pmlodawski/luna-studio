---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Analysis.MaxID.MaxID where

import           Flowbox.Luna.Data.AST.Expr               (Expr)
import qualified Flowbox.Luna.Data.AST.Expr               as Expr
import           Flowbox.Luna.Data.AST.Lit                (Lit)
import qualified Flowbox.Luna.Data.AST.Lit                as Lit
import           Flowbox.Luna.Data.AST.Module             (Module)
import qualified Flowbox.Luna.Data.AST.Module             as Module
import           Flowbox.Luna.Data.AST.Pat                (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                as Pat
import           Flowbox.Luna.Data.AST.Type               (Type)
import qualified Flowbox.Luna.Data.AST.Type               as Type
import qualified Flowbox.Luna.Data.AST.Utils              as AST
import           Flowbox.Luna.Passes.Analysis.MaxID.State (MaxIDState)
import qualified Flowbox.Luna.Passes.Analysis.MaxID.State as State
import           Flowbox.Luna.Passes.Pass                 (PassMonad)
import qualified Flowbox.Luna.Passes.Pass                 as Pass
import           Flowbox.Prelude                          hiding (mapM, mapM_)
import           Flowbox.System.Log.Logger


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Analysis.MaxID.MaxID"


type MaxIDMonad m = PassMonad MaxIDState m


run :: PassMonad s m => Module -> Pass.Result m AST.ID
run = (Pass.run_ (Pass.Info "MaxID") $ State.make) . analyseModule


analyseModule :: MaxIDMonad m => Module -> Pass.Result m AST.ID
analyseModule m = do State.reportID i
                     _ <- Module.traverseM_ analyseModule analyseExpr analyseType analysePat analyseLit m
                     State.getMaxID
    where i = m ^. Module.id


analyseExpr :: MaxIDMonad m => Expr -> Pass.Result m ()
analyseExpr e = do State.reportID i
                   Expr.traverseM_ analyseExpr analyseType analysePat analyseLit e
    where i = e ^. Expr.id


analysePat :: MaxIDMonad m => Pat -> Pass.Result m ()
analysePat p = do State.reportID i
                  Pat.traverseM_ analysePat analyseType analyseLit p
    where i = p ^. Pat.id


analyseType :: MaxIDMonad m => Type -> Pass.Result m ()
analyseType t = do State.reportID i
                   Type.traverseM_ analyseType t
    where i = t ^. Type.id


analyseLit :: MaxIDMonad m => Lit -> Pass.Result m ()
analyseLit l = State.reportID i
    where i = l ^. Lit.id

