---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Transform.AST.IDFixer.IDFixer where

import           Flowbox.Luna.Data.AST.Expr                      (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                      as Expr
import           Flowbox.Luna.Data.AST.Lit                       (Lit)
import qualified Flowbox.Luna.Data.AST.Lit                       as Lit
import           Flowbox.Luna.Data.AST.Module                    (Module)
import qualified Flowbox.Luna.Data.AST.Module                    as Module
import           Flowbox.Luna.Data.AST.Pat                       (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                       as Pat
import           Flowbox.Luna.Data.AST.Type                      (Type)
import qualified Flowbox.Luna.Data.AST.Type                      as Type
import qualified Flowbox.Luna.Data.AST.Utils                     as AST
import           Flowbox.Luna.Data.AST.Zipper.Focus              (Focus)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus              as Focus
import           Flowbox.Luna.Passes.Pass                        (PassMonad)
import qualified Flowbox.Luna.Passes.Pass                        as Pass
import           Flowbox.Luna.Passes.Transform.AST.IDFixer.State (IDFixerState)
import qualified Flowbox.Luna.Passes.Transform.AST.IDFixer.State as State
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.AST.IDFixer.IDFixer"


type IDFixerMonad m = PassMonad IDFixerState m


run :: PassMonad s m => AST.ID -> Bool -> Focus -> Pass.Result m Focus
run maxID fixAll = (Pass.run_ (Pass.Info "IDFixer") $ State.make maxID fixAll) . fixFocus


runModule :: PassMonad s m => AST.ID -> Bool -> Module -> Pass.Result m Module
runModule maxID fixAll = (Pass.run_ (Pass.Info "IDFixer") $ State.make maxID fixAll) . fixModule


runExpr :: PassMonad s m => AST.ID -> Bool -> Expr -> Pass.Result m Expr
runExpr maxID fixAll = (Pass.run_ (Pass.Info "IDFixer") $ State.make maxID fixAll) . fixExpr


runExprs :: PassMonad s m => AST.ID -> Bool -> [Expr] -> Pass.Result m [Expr]
runExprs maxID fixAll = (Pass.run_ (Pass.Info "IDFixer") $ State.make maxID fixAll) . (mapM fixExpr)


runType :: PassMonad s m => AST.ID -> Bool -> Type -> Pass.Result m Type
runType maxID fixAll = (Pass.run_ (Pass.Info "IDFixer") $ State.make maxID fixAll) . fixType


fixFocus :: IDFixerMonad m => Focus -> Pass.Result m Focus
fixFocus f = Focus.traverseM fixModule fixExpr f


fixModule :: IDFixerMonad m => Module -> Pass.Result m Module
fixModule m = do n <- State.fixID $ m ^. Module.id
                 Module.traverseM fixModule fixExpr fixType fixPat fixLit $ m & Module.id .~ n


fixExpr :: IDFixerMonad m => Expr -> Pass.Result m Expr
fixExpr e = do n <- State.fixID $ e ^. Expr.id
               Expr.traverseM fixExpr fixType fixPat fixLit $ e & Expr.id .~ n


fixPat :: IDFixerMonad m => Pat -> Pass.Result m Pat
fixPat p = do n <- State.fixID $ p ^. Pat.id
              Pat.traverseM fixPat fixType fixLit $ p & Pat.id .~ n


fixType :: IDFixerMonad m => Type -> Pass.Result m Type
fixType t = do n <- State.fixID $ t ^. Type.id
               Type.traverseM fixType $ t & Type.id .~ n


fixLit :: IDFixerMonad m => Lit -> Pass.Result m Lit
fixLit l = do n <- State.fixID $ l ^. Lit.id
              return $ l & Lit.id .~ n

