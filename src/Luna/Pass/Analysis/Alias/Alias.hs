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
import           Luna.Data.Namespace.State      (regModule, withScope, getAliasInfo, regExpr, regPat, regType, regLit, regVarName, bindVar, regTypeName)
import           Luna.Pass.Pass                 (Pass)
import qualified Luna.Pass.Pass                 as Pass
import qualified Luna.Data.Namespace            as Namespace
import           Luna.Data.Namespace       (Namespace)


logger :: LoggerIO
logger = getLoggerIO $(moduleName)


type VAPass result = Pass Namespace result


run :: Module -> Pass.Result AliasInfo
run = (Pass.run_ (Pass.Info "Alias") mempty) . vaMod


vaMod :: Module -> VAPass AliasInfo
vaMod el@(Module.Module id cls imports classes typeAliases typeDefs fields methods modules) = do
    regModule el
    withScope id $ regVarName name id *> continue
    getAliasInfo
    where name     = el ^. Module.cls ^. Type.name
          continue =  pure ()
                   -- -- <* mapM registerDataCons classes -- register just data constructors before functions
                   <* mapM registerFuncHeaders methods
                   <* mapM registerClassHeaders classes

                   <* vaType cls
                   <* fexpMap imports
                   <* fexpMap classes -- register class functions before data member functions
                   <* fexpMap typeAliases
                   <* fexpMap typeDefs
                   <* fexpMap fields
                   <* fexpMap methods
                   <* fmodMap modules
          fexpMap  = mapM vaExpr
          fmodMap  = mapM vaMod


--registerDataCons :: Expr.Expr -> VAPass ()
--registerDataCons el = VAState.regExpr el *> case el of
--    --Expr.Data       {} -> withID continue
--    --Expr.ConD       {} -> regParentVarName name id *> continue
--    _                  -> continue
--    where continue = Expr.traverseM_ registerDataCons vaType vaPat vaLit el
--          withID   = VAState.withID (el ^. Expr.id)
--          id       = el ^.  Expr.id
--          name     = el ^.  Expr.name

registerClassHeaders :: Expr.Expr -> VAPass ()
registerClassHeaders cls = case cls of
    Expr.Data       id cls cons _ _ -> register' id cls cons
    Expr.DataNative id cls cons _ _ -> register' id cls cons
    where register' id cls cons = regTypeName name id <* mapM registerConsHeaders cons
                                  where name = view Type.name cls

registerConsHeaders :: Expr.Expr -> VAPass ()
registerConsHeaders (Expr.ConD id name fields) = regVarName name id



registerFuncHeaders :: Expr.Expr -> VAPass ()
registerFuncHeaders el = regExpr el *> case el of
    Expr.Function   id _ name _ _ _ -> regVarName (Name.unified name) id
    where continue = Expr.traverseM_ registerFuncHeaders vaType vaPat vaLit pure el


vaExpr :: Expr.Expr -> VAPass ()
vaExpr el = regExpr el *> case el of
    Expr.Lambda     {}              -> withScope id continue
    Expr.Cond       {}              -> withScope id continue
    Expr.Data       {}              -> withScope id continue
    Expr.DataNative {}              -> withScope id continue
    Expr.Function   id _ name _ _ _ -> withScope id continue
    Expr.Var        id name         -> bindVar id name
    Expr.FuncVar    id name         -> bindVar id (Name.unified name)
    Expr.NativeVar  id name         -> bindVar id name
    Expr.Con        id name         -> bindVar id name
    Expr.ConD       {}              -> continue
    Expr.Field      id name _ _     -> regVarName name id *> continue
    Expr.Assignment id pat dst      -> vaExpr dst <* vaPat pat
    _                  -> continue
    where continue = Expr.traverseM_ vaExpr vaType vaPat vaLit pure el
          id       = el ^.  Expr.id


vaPat :: Pat -> VAPass ()
vaPat el = regPat el *> case el of
    Pat.Var     id name                 -> regVarName name id
    _                                   -> Pat.traverseM_ vaPat vaType vaLit el


vaType :: Type -> VAPass ()
vaType el = regType el *> case el of
    _                                   -> Type.traverseM_ vaType el


vaLit :: Lit -> VAPass ()
vaLit el = regLit el
