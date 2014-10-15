---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.Pass.Analysis.Alias.Alias where

import Control.Applicative

import           Flowbox.Prelude                hiding (error, id, mod)
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Expr                  as Expr
import           Luna.AST.Lit                   (Lit)
import           Luna.AST.Module                (Module)
import qualified Luna.AST.Module                as Module
import qualified Luna.AST.Name                  as Name
import           Luna.AST.Pat                   (Pat)
import qualified Luna.AST.Pat                   as Pat
import           Luna.AST.Type                  (Type)
import qualified Luna.AST.Type                  as Type
import           Luna.Data.AliasInfo            (AliasInfo)
import           Luna.Pass.Analysis.Alias.State (VAState, bindVar, regParentVarName, regTypeName, regVarName)
import qualified Luna.Pass.Analysis.Alias.State as VAState
import           Luna.Pass.Pass                 (Pass)
import qualified Luna.Pass.Pass                 as Pass



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


type VAPass result = Pass VAState result


run :: Module -> Pass.Result AliasInfo
run = (Pass.run_ (Pass.Info "Alias") mempty) . vaMod


vaMod :: Module -> VAPass AliasInfo
vaMod el@(Module.Module id cls imports classes typeAliases typeDefs fields methods modules) = do
    VAState.registerModule el
    withID $ do regVarName name id
                continue
    VAState.getAliasInfo
    where continue =  pure ()
                   -- <* mapM registerDataCons classes -- register just data constructors before functions
                   <* mapM registerFuncHeaders methods

                   <* vaType cls
                   <* fexpMap imports
                   <* fexpMap classes -- register functions before data member functions
                   <* fexpMap typeAliases
                   <* fexpMap typeDefs
                   <* fexpMap fields
                   <* fexpMap methods
                   <* fmodMap modules
          withID   = VAState.withID id
          id       = el ^. Module.id
          name     = el ^. Module.cls ^. Type.name
          fexpMap  = mapM vaExpr
          fmodMap  = mapM vaMod


registerDataCons :: Expr.Expr -> VAPass ()
registerDataCons el = VAState.registerExpr el *> case el of
    Expr.Data       {} -> withID continue
    Expr.ConD       {} -> regParentVarName name id *> continue
    _                  -> continue
    where continue = Expr.traverseM_ registerDataCons vaType vaPat vaLit pure el
          withID   = VAState.withID (el ^. Expr.id)
          id       = el ^.  Expr.id
          name     = el ^.  Expr.name


registerFuncHeaders :: Expr.Expr -> VAPass ()
registerFuncHeaders el = VAState.registerExpr el *> case el of
    Expr.Function   _ _ name _ _ _ -> regVarName (Name.unified name) id       *> withID continue
    _                  -> continue
    where continue = Expr.traverseM_ registerFuncHeaders vaType vaPat vaLit pure el
          withID   = VAState.withID (el ^. Expr.id)
          id       = el ^.  Expr.id
          name     = el ^.  Expr.name


vaExpr :: Expr.Expr -> VAPass ()
vaExpr el = VAState.registerExpr el *> case el of
    Expr.Lambda     {} -> withID continue
    Expr.Cond       {} -> withID continue
    Expr.Data       {} -> regTypeName name id *> withID continue
    Expr.DataNative {} -> withID continue
    Expr.Var        {} -> bindVar id name
    Expr.NativeVar  {} -> bindVar id name
    Expr.Con        {} -> bindVar id name
    Expr.ConD       {} -> regParentVarName name id *> continue
    Expr.Function   _ _ name _ _ _ -> regVarName (Name.unified name) id       *> withID continue
    Expr.Field      {} -> regVarName name id       *> continue
    Expr.Assignment {} -> vaExpr dst <* vaPat pat
    _                  -> continue
    where continue = Expr.traverseM_ vaExpr vaType vaPat vaLit pure el
          withID   = VAState.withID (el ^. Expr.id)
          id       = el ^.  Expr.id
          name     = el ^.  Expr.name
          pat      = el ^?! Expr.pat
          dst      = el ^?! Expr.dst


vaPat :: Pat -> VAPass ()
vaPat el = VAState.registerPat el *> case el of
    Pat.Var     id name                 -> regVarName name id
    _                                   -> Pat.traverseM_ vaPat vaType vaLit el


vaType :: Type -> VAPass ()
vaType el = VAState.registerType el *> case el of
    _                                   -> Type.traverseM_ vaType el


vaLit :: Lit -> VAPass ()
vaLit el = VAState.registerLit el
