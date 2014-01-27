---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}

module Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias where

import Control.Applicative

import           Flowbox.Luna.Data.Analysis.Alias.Alias      (AA)
import qualified Flowbox.Luna.Data.AST.Expr                  as Expr
import           Flowbox.Luna.Data.AST.Module                (Module)
import qualified Flowbox.Luna.Data.AST.Module                as Module
import           Flowbox.Luna.Data.AST.Pat                   (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                   as Pat
import           Flowbox.Luna.Data.AST.Type                  (Type)
import qualified Flowbox.Luna.Data.AST.Type                  as Type
import           Flowbox.Luna.Passes.Analysis.VarAlias.State (VAState)
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.State as VAState
import           Flowbox.Luna.Passes.Pass                    (Pass)
import qualified Flowbox.Luna.Passes.Pass                    as Pass
import           Flowbox.Prelude                             hiding (error, id, mod)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.VarAlias.VarAlias"


type VAPass result = Pass VAState result


run :: Module -> Pass.Result AA
run = (Pass.run_ (Pass.Info "VarAlias") mempty) . vaMod


vaMod :: Module -> VAPass AA
vaMod mod = VAState.withID (mod ^. Module.id) $ do
    Module.traverseM_ vaMod vaExpr vaType vaPat pure mod
    VAState.getAA


vaExpr :: Expr.Expr -> VAPass ()
vaExpr el = VAState.registerID (el ^. Expr.id) *> case el of
    Expr.Function   id _ _ inputs _ body  -> VAState.withID id $ do
                                             exprMap inputs
                                             exprMap body
    Expr.Lambda     id inputs _ body      -> VAState.withID id $ do
                                             exprMap inputs
                                             exprMap body
    Expr.Assignment _ pat dst             -> vaExpr dst <* vaPat pat
    Expr.Con        id name               -> VAState.bindVar id name
    Expr.Var        id name               -> VAState.bindVar id name
    Expr.NativeVar  id name               -> VAState.bindVar id name
    Expr.ConD       id name _             -> VAState.registerVarName name id
    _                                     -> continue
    where exprMap  = mapM_ vaExpr
          continue = Expr.traverseM_ vaExpr vaType vaPat pure el


vaPat :: Pat -> VAPass ()
vaPat el = VAState.registerID (el ^. Pat.id) *> case el of
    Pat.Var     id name                 -> do
                                           VAState.registerVarName name id
    _                                   -> Pat.traverseM_ vaPat vaType pure el


vaType :: Type -> VAPass ()
vaType el = VAState.registerID (el ^. Type.id) *> case el of
    --Type.Tuple  _ items                 -> mapM vaType items *> return ()
    _                                   -> Type.traverseM_ vaType el
