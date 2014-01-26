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

import           Control.Applicative
import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.IntMap         as IntMap

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
import           Flowbox.Luna.Passes.Analysis.VarAlias.State    (VAState)
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.State    as VAState
import           Flowbox.Luna.Passes.Pass                       (Pass, PassT)
import qualified Flowbox.Luna.Passes.Pass                       as Pass
import           Flowbox.Prelude                                hiding (error, id, mod)
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.VarAlias.VarAlias"


type VAPass result = Pass VAState result


--run :: Module -> Pass.Result AA
--run = (Pass.run_ (Pass.Info "VarAlias") mempty) . gatherAndCheck


--runGather :: Module -> Pass.Result AA
--runGather = (Pass.run_ (Pass.Info "VarAlias") mempty) . vaMod

run :: Module -> Pass.Result AA
run = (Pass.run_ (Pass.Info "VarAlias") mempty) . vaMod


--gatherAndCheck :: Module -> VAPass AA
--gatherAndCheck m = do gvm <- vaMod m
--                      (AA . IntMap.fromList) <$> (mapM check $ IntMap.toList $ AA.varmap gvm)


--check :: (Int, Either String Int) -> VAPass (Int, Int)
--check (id, v) = case v of
--    Left e   -> fail e
--    Right vi -> return (id, vi)


--runNested :: (MonadIO m, MonadState AA m) => PassT AA result (Pass.ResultT m) -> Pass.ResultT m AA
--runNested p = do
--    s <- get
--    snd <$> Pass.runHoist (Pass.Info "VarAlias") s p



vaMod :: Module -> VAPass AA
vaMod mod = do
    VAState.switchID (mod ^. Module.id)
    Module.traverseM_ vaMod vaExpr vaType vaPat pure mod
    VAState.getAA


vaExpr :: Expr.Expr -> VAPass ()
vaExpr el = VAState.registerID (el ^. Expr.id) *> case el of
    Expr.Function   _ _ _ inputs _ body  -> do
                                            VAState.switchID (el ^. Expr.id)
                                            exprMap inputs
                                            exprMap body
    Expr.Assignment _ pat dst            -> vaExpr dst <* vaPat pat
    Expr.Con        id name              -> VAState.bindVar id name
    Expr.Var        id name              -> VAState.bindVar id name
    Expr.NativeVar  id name              -> VAState.bindVar id name
    Expr.ConD       id name _            -> VAState.registerVarName name id
    _                                    -> continue
    where
        exprMap  = mapM_ vaExpr
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
