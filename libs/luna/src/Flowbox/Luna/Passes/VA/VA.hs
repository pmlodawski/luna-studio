---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.VA.VA where

import qualified Flowbox.Luna.AST.Expr         as Expr
import qualified Flowbox.Luna.AST.Type         as Type
import           Flowbox.Luna.AST.Type           (Type)
import qualified Flowbox.Luna.AST.Pat          as Pat
import           Flowbox.Luna.AST.Pat            (Pat)
import qualified Flowbox.Luna.Passes.VA.State as LocState
import           Flowbox.Luna.Passes.VA.State   (LocState)
import           Flowbox.Luna.Passes.VA.State   (VarStat)
import qualified Flowbox.Luna.Passes.Pass      as Pass
import           Flowbox.Luna.Passes.Pass        (PassMonad)

import           Control.Monad.State             
import           Control.Applicative             

import           Flowbox.System.Log.Logger       

import qualified Flowbox.Prelude               as Prelude
import           Flowbox.Prelude               hiding (error)

import Debug.Trace


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.VA.VA"


type SSAMonad m = PassMonad LocState m


run :: PassMonad s m => Expr.Expr -> Pass.Result m VarStat
run = (Pass.run_ LocState.empty) . ssaExpr


ssaExpr :: SSAMonad m => Expr.Expr -> Pass.Result m VarStat
ssaExpr ast = do
    ssaAST ast
    LocState.varstat <$> get


runNested :: SSAMonad m => Pass.Transformer LocState b -> Pass.Result m LocState
runNested f = do
    s <- get
    Pass.run'_ s f


ssaAST :: SSAMonad m => Expr.Expr -> Pass.Result m ()
ssaAST ast = case ast of
    Expr.Function   id name signature body -> do
                                              s <- runNested $ do
                                                  mapM ssaPat signature
                                                  ssaExprMap body
                                              LocState.updateVarStat s
    Expr.Assignment id pat dst             -> ssaAST dst <* ssaPat pat
    Expr.Var        id name                -> do
                                              v <- LocState.lookupVar name
                                              case v of
                                                  Nothing    -> return () 
                                                  Just vid   -> LocState.bind id vid
    Expr.Class      id cls classes fields 
                    methods                -> () <$ do 
                                                 ssaType cls
                                                 ssaExprMap classes 
                                                 ssaExprMap fields 
                                                 ssaExprMap methods
    _                                      -> Expr.traverseM_ ssaAST ast
    where
        ssaExprMap = mapM ssaAST



ssaPat :: SSAMonad m => Pat -> Pass.Result m ()
ssaPat pat = case pat of
    Pat.Var     id name                 -> LocState.registerVarName (name, id)
    Pat.Wildcard id                     -> return ()
    _                                   -> logger error "SSA Pass error: Unknown pattern." *> Pass.fail "Unknown pattern"

ssaType :: SSAMonad m => Type -> Pass.Result m ()
ssaType ast = case ast of
    Type.Tuple  id items          -> mapM ssaType items *> return ()
    Type.Var    id name           -> return ()
    _                             -> logger error "SSA Pass error: Unknown type." *> Pass.fail "Unknown type"
