---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}

module Flowbox.Luna.Passes.Analysis.Alias.Alias where

import Control.Applicative

import qualified Flowbox.Luna.Data.AST.Expr               as Expr
import           Flowbox.Luna.Data.AST.Lit                (Lit)
import qualified Flowbox.Luna.Data.AST.Lit                as Lit
import           Flowbox.Luna.Data.AST.Module             (Module)
import qualified Flowbox.Luna.Data.AST.Module             as Module
import           Flowbox.Luna.Data.AST.Pat                (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                as Pat
import           Flowbox.Luna.Data.AST.Type               (Type)
import qualified Flowbox.Luna.Data.AST.Type               as Type
import           Flowbox.Luna.Data.Pass.AliasInfo         (AliasInfo)
import           Flowbox.Luna.Passes.Analysis.Alias.State (VAState)
import qualified Flowbox.Luna.Passes.Analysis.Alias.State as VAState
import           Flowbox.Luna.Passes.Pass                 (Pass)
import qualified Flowbox.Luna.Passes.Pass                 as Pass
import           Flowbox.Prelude                          hiding (error, id, mod)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.Alias.Alias"


type VAPass result = Pass VAState result


run :: Module -> Pass.Result AliasInfo
run = (Pass.run_ (Pass.Info "Alias") mempty) . vaMod


vaMod :: Module -> VAPass AliasInfo
vaMod el = do VAState.registerModule el
              withID $ do VAState.registerVarName name id
                          continue
              VAState.getAliasInfo
           where continue = Module.traverseM_ vaMod vaExpr vaType vaPat vaLit el
                 withID   = VAState.withID id
                 id       = el ^. Module.id
                 name     = el ^. Module.cls ^. Type.name


vaExpr :: Expr.Expr -> VAPass ()
vaExpr el = VAState.registerExpr el *> case el of
    Expr.Function   {} -> VAState.registerVarName name id *> withID continue
    Expr.Lambda     {} -> withID continue
    Expr.Cond       {} -> withID continue
    Expr.Assignment {} -> vaExpr dst <* vaPat pat
    Expr.Con        {} -> VAState.bindVar id name
    Expr.Var        {} -> VAState.bindVar id name
    Expr.NativeVar  {} -> VAState.bindVar id name
    Expr.ConD       {} -> VAState.registerVarName name id
    _                  -> continue
    where continue = Expr.traverseM_ vaExpr vaType vaPat vaLit el
          withID   = VAState.withID (el ^. Expr.id)
          id       = el ^.  Expr.id
          name     = el ^.  Expr.name
          pat      = el ^?! Expr.pat
          dst      = el ^?! Expr.dst


vaPat :: Pat -> VAPass ()
vaPat el = VAState.registerPat el *> case el of
    Pat.Var     id name                 -> VAState.registerVarName name id
    _                                   -> Pat.traverseM_ vaPat vaType vaLit el


vaType :: Type -> VAPass ()
vaType el = VAState.registerType el *> case el of
    _                                   -> Type.traverseM_ vaType el


vaLit :: Lit -> VAPass ()
vaLit el = VAState.registerLit el
