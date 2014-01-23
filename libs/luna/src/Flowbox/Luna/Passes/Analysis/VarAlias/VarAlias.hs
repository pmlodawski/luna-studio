---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE Rank2Types #-}

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
import qualified Flowbox.Luna.Passes.Pass                       as Pass
import           Flowbox.Luna.Passes.Pass                      (Pass, PassT)
import           Flowbox.Prelude                                hiding (error, id, mod)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.VarAlias.VarAlias"


type VAPass result = Pass LocState result


run :: Module -> Pass.Result AA
run = (Pass.run_ (Pass.Info "VarAlias") LocState.empty) . gatherAndCheck


runGather :: Module -> Pass.Result GeneralVarMap
runGather = (Pass.run_ (Pass.Info "VarAlias") LocState.empty) . vaMod


gatherAndCheck :: Module -> VAPass AA
gatherAndCheck m = do gvm <- vaMod m
                      (AA . IntMap.fromList) <$> (mapM check $ IntMap.toList $ GeneralVarMap.varmap gvm)


check :: (Int, Either String Int) -> VAPass (Int, Int)
check (id, v) = case v of
    Left e   -> fail e
    Right vi -> return (id, vi)


runNested :: (MonadIO m, MonadState LocState m) => PassT LocState result (Pass.ResultT m) -> Pass.ResultT m LocState
runNested p = do
    s <- get
    snd <$> Pass.runHoist (Pass.Info "VarAlias") s p


vaMod :: Module -> VAPass GeneralVarMap
vaMod mod = do
    Module.traverseM_ vaMod vaExpr vaType vaPat pure mod
    LocState.varstat <$> get


vaExpr :: Expr.Expr -> VAPass ()
vaExpr ast = case ast of
    Expr.Function   _ _ _ inputs _ body   -> do
                                             s <- runNested $ do
                                                  mapM_ vaExpr inputs
                                                  vaExprMap body
                                             LocState.updateVarStat s
    Expr.Assignment _ pat dst             -> vaExpr dst <* vaPat pat
    Expr.Var        id name               -> LocState.bindVar name id
    Expr.NativeVar  id name               -> LocState.bindVar name id
    _                                     -> continue
    where
        vaExprMap = mapM_ vaExpr
        continue  = Expr.traverseM_ vaExpr vaType vaPat pure ast



vaPat :: Pat -> VAPass ()
vaPat pat = case pat of
    Pat.Var     id name                 -> LocState.registerVarName (name, id)
    Pat.Wildcard _                      -> return ()
    _                                   -> Pat.traverseM_ vaPat vaType pure pat

vaType :: Type -> VAPass ()
vaType t = case t of
    Type.Tuple  _ items                 -> mapM vaType items *> return ()
    _                                   -> Type.traverseM_ vaType t
