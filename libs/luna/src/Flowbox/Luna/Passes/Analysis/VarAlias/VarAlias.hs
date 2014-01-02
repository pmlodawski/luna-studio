---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

module Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias where

import           Control.Applicative
import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.IntMap            as IntMap

import           Flowbox.Luna.Data.Analysis.Alias.Alias         (AA (AA))
import           Flowbox.Luna.Data.Analysis.Alias.GeneralVarMap (GeneralVarMap)
import qualified Flowbox.Luna.Data.Analysis.Alias.GeneralVarMap as GeneralVarMap
import qualified Flowbox.Luna.Data.AST.Expr                     as Expr
import           Flowbox.Luna.Data.AST.Module                   (Module)
import qualified Flowbox.Luna.Data.AST.Module                   as Module
import           Flowbox.Luna.Data.AST.Pat                      (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                      as Pat
import           Flowbox.Luna.Data.AST.Type                     (Type)
import qualified Flowbox.Luna.Data.AST.Type                     as Type
import           Flowbox.Luna.Passes.Analysis.VarAlias.State    (LocState)
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.State    as LocState
import           Flowbox.Luna.Passes.Pass                       (PassMonad)
import qualified Flowbox.Luna.Passes.Pass                       as Pass
import           Flowbox.Prelude                                hiding (error, id, mod)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.VarAlias.VarAlias"


type VAMonad m = PassMonad LocState m


run :: PassMonad s m => Module -> Pass.Result m AA
run = (Pass.run_ (Pass.Info "VarAlias") LocState.empty) . gatherAndCheck


runGather :: PassMonad s m => Module -> Pass.Result m GeneralVarMap
runGather = (Pass.run_ (Pass.Info "VarAlias") LocState.empty) . vaMod


gatherAndCheck :: VAMonad m => Module -> Pass.Result m AA
gatherAndCheck m = do gvm <- vaMod m
                      (AA . IntMap.fromList) <$> (mapM check $ IntMap.toList $ GeneralVarMap.varmap gvm)


check :: VAMonad m => (Int, Either String Int) -> Pass.Result m (Int, Int)
check (id, v) = case v of
    Left e   -> fail e
    Right vi -> return (id, vi)


runNested :: VAMonad m => Pass.Transformer LocState b -> Pass.Result m LocState
runNested f = do
    s <- get
    Pass.run'_ (Pass.Info "VarAlias") s f


vaMod :: VAMonad m => Module -> Pass.Result m GeneralVarMap
vaMod mod = do
    Module.traverseM_ vaMod vaExpr vaType vaPat pure mod
    LocState.varstat <$> get


vaExpr :: VAMonad m => Expr.Expr -> Pass.Result m ()
vaExpr ast = case ast of
    Expr.Function   _ _ _ inputs _ body   -> do
                                             s <- runNested $ do
                                                  mapM_ vaExpr inputs
                                                  vaExprMap body
                                             LocState.updateVarStat s
    Expr.Assignment _ pat dst             -> vaExpr dst <* vaPat pat
    Expr.Var        id name               -> LocState.bindVar name id
    Expr.NativeVar  id name               -> LocState.bindVar name id
    _                                     -> Expr.traverseM_ vaExpr vaType vaPat pure ast
    where
        vaExprMap = mapM_ vaExpr



vaPat :: VAMonad m => Pat -> Pass.Result m ()
vaPat pat = case pat of
    Pat.Var     id name                 -> LocState.registerVarName (name, id)
    Pat.Wildcard _                      -> return ()
    _                                   -> Pat.traverseM_ vaPat vaType pure pat

vaType :: VAMonad m => Type -> Pass.Result m ()
vaType t = case t of
    Type.Tuple  _ items                 -> mapM vaType items *> return ()
    _                                   -> Type.traverseM_ vaType t
