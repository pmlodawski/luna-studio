---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias where

import qualified Flowbox.Luna.Data.AST.Expr                  as Expr
import qualified Flowbox.Luna.Data.AST.Type                  as Type
import           Flowbox.Luna.Data.AST.Type                    (Type)
import qualified Flowbox.Luna.Data.AST.Pat                   as Pat
import           Flowbox.Luna.Data.AST.Pat                     (Pat)
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.State as LocState
import           Flowbox.Luna.Passes.Analysis.VarAlias.State   (LocState)
import           Flowbox.Luna.Data.AliasAnalysis               (AA)
import qualified Flowbox.Luna.Passes.Pass                    as Pass
import           Flowbox.Luna.Passes.Pass                      (PassMonad)

import           Control.Monad.State                           
import           Control.Applicative                           

import           Flowbox.System.Log.Logger                     

import           Flowbox.Prelude                             hiding (error, id)



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.VarAlias.VarAlias"


type VAMonad m = PassMonad LocState m


run :: PassMonad s m => Expr.Expr -> Pass.Result m AA
run = (Pass.run_ LocState.empty) . vaExpr


vaExpr :: VAMonad m => Expr.Expr -> Pass.Result m AA
vaExpr ast = do
    vaAST ast
    LocState.varstat <$> get


runNested :: VAMonad m => Pass.Transformer LocState b -> Pass.Result m LocState
runNested f = do
    s <- get
    Pass.run'_ s f


vaAST :: VAMonad m => Expr.Expr -> Pass.Result m ()
vaAST ast = case ast of
    Expr.Function   _ _ pats _ body       -> do
                                             s <- runNested $ do
                                                 mapM_ vaPat pats
                                                 vaExprMap body
                                             LocState.updateVarStat s
    Expr.Assignment _ pat dst             -> vaAST dst <* vaPat pat
    Expr.Accessor   _ src _               -> vaAST src -- we cannot determine if dst exists.
    Expr.Var        id name               -> do
                                             v <- LocState.lookupVar name
                                             case v of
                                                 Nothing    -> logger error ("Not in scope '" ++ name ++ "'.") *> Pass.fail ("not in scope '" ++ name ++ "'")
                                                 Just vid   -> LocState.bind id vid
    _                                     -> Expr.traverseM_ vaAST vaType vaPat pure ast
    where
        vaExprMap = mapM_ vaAST



vaPat :: VAMonad m => Pat -> Pass.Result m ()
vaPat pat = case pat of
    Pat.Var     id name                 -> LocState.registerVarName (name, id)
    Pat.Wildcard _                      -> return ()
    _                                   -> Pat.traverseM_ vaPat vaType pure pat

vaType :: VAMonad m => Type -> Pass.Result m ()
vaType t = case t of
    Type.Tuple  _ items                 -> mapM vaType items *> return ()
    _                                   -> Type.traverseM_ vaType t
