---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}

module Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias where

import qualified Flowbox.Luna.Data.AST.Expr                  as Expr
import qualified Flowbox.Luna.Data.AST.Type                  as Type
import           Flowbox.Luna.Data.AST.Type                    (Type)
import qualified Flowbox.Luna.Data.AST.Pat                   as Pat
import           Flowbox.Luna.Data.AST.Pat                     (Pat)
import qualified Flowbox.Luna.Data.AST.Module                as Module
import           Flowbox.Luna.Data.AST.Module                  (Module)
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.State as LocState
import           Flowbox.Luna.Passes.Analysis.VarAlias.State   (LocState)
import           Flowbox.Luna.Data.AliasAnalysis               (AA)
import qualified Flowbox.Luna.Passes.Pass                    as Pass
import           Flowbox.Luna.Passes.Pass                      (Pass, PassT)

import           Control.Monad.State                         hiding (mapM, mapM_)
import           Control.Applicative                           

import           Flowbox.System.Log.Logger                     

import           Flowbox.Prelude                             hiding (error, id, mod)



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.VarAlias.VarAlias"


type VAPass result = Pass LocState result


run :: Module -> Pass.Result AA
run = (Pass.run_ (Pass.Info "VarAlias") LocState.empty) . vaMod


runNested :: (MonadIO m, MonadState LocState m) => PassT LocState result (Pass.ResultT m) -> Pass.ResultT m LocState
runNested p = do
    s <- get
    snd <$> Pass.runHoist (Pass.Info "VarAlias") s p


vaMod :: Module -> VAPass AA
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
    Expr.Var        id name               -> do
                                             v <- LocState.lookupVar name
                                             case v of
                                                 Nothing    -> logger error ("Not in scope '" ++ name ++ "'.")
                                                 Just vid   -> LocState.bind id vid
    Expr.NativeVar  id name               -> do
                                             v <- LocState.lookupVar name
                                             case v of
                                                 Nothing    -> logger error ("Not in scope '" ++ name ++ "'.")
                                                 Just vid   -> LocState.bind id vid
    _                                     -> Expr.traverseM_ vaExpr vaType vaPat pure ast
    where
        vaExprMap = mapM_ vaExpr



vaPat :: Pat -> VAPass ()
vaPat pat = case pat of
    Pat.Var     id name                 -> LocState.registerVarName (name, id)
    Pat.Wildcard _                      -> return ()
    _                                   -> Pat.traverseM_ vaPat vaType pure pat

vaType :: Type -> VAPass ()
vaType t = case t of
    Type.Tuple  _ items                 -> mapM vaType items *> return ()
    _                                   -> Type.traverseM_ vaType t
