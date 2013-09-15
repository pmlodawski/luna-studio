---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.SSA.SSA where

import qualified Flowbox.Luna.AST.Expr         as Expr
import qualified Flowbox.Luna.AST.Type         as Type
import           Flowbox.Luna.AST.Type           (Type)
import qualified Flowbox.Luna.AST.Pat          as Pat
import           Flowbox.Luna.AST.Pat            (Pat)
import qualified Flowbox.Luna.Passes.SSA.State as SSAState
import           Flowbox.Luna.Passes.SSA.State   (SSAState)
import qualified Flowbox.Luna.Passes.Pass      as Pass
import           Flowbox.Luna.Passes.Pass        (PassMonad)

import           Control.Monad.State             
import           Control.Applicative             

import           Flowbox.System.Log.Logger       

import qualified Flowbox.Prelude               as Prelude
import           Flowbox.Prelude               hiding (error)


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.SSA.SSA"


type SSAMonad m = PassMonad SSAState m


run :: PassMonad s m => Expr.Expr -> Pass.Result m Expr.Expr
run = (Pass.runM SSAState.empty) . ssaAST


runNested :: SSAMonad m => Pass.Transformer SSAState a m b 
runNested f = do
    s <- get
    Pass.runM s f


ssaAST :: SSAMonad m => Expr.Expr -> Pass.Result m Expr.Expr
ssaAST ast = case ast of
    Expr.Function   id name signature body -> runNested $ do
                                                  SSAState.registerVar (name, name)
                                                  mapM ssaPat signature
                                                  Expr.Function id name signature <$> ssaExprMap body
    Expr.Assignment id src dst             -> (flip (Expr.Assignment id) <$> ssaAST dst <*> ssaAST src)
    Expr.Var        id name                -> do
                                              v <- SSAState.lookupVar name
                                              case v of
                                                  Nothing    -> (logger error $ "Not in scope: '" ++ name ++ "'") *> Pass.fail "Not in scope"
                                                  Just lname -> return $ Expr.Var id lname
    Expr.Class      id cls classes fields 
                    methods                -> do ssaType cls
                                                 Expr.Class id cls <$> ssaExprMap classes 
                                                                   <*> ssaExprMap fields 
                                                                   <*> ssaExprMap methods
    _                                      -> Expr.traverseM ssaAST ast
    where
        ssaExprMap = mapM ssaAST

ssaPat :: SSAMonad m => Pat -> Pass.Result m Pat
ssaPat pat = case pat of
    Pat.Var     id name                 -> Pat.Var id <$> SSAState.handleVar name
    _                                   -> logger error "SSA Pass error: Unknown pattern." *> Pass.fail "Unknown pattern"

ssaType :: SSAMonad m => Type -> Pass.Result m ()
ssaType ast = case ast of
    Type.Tuple  id items          -> mapM ssaType items *> return ()
    Type.Var    id name           -> SSAState.registerVar (name, name)
    Type.Class  id name   _       -> SSAState.registerVar (name, name)
    _                             -> logger error "SSA Pass error: Unknown type." *> Pass.fail "Unknown type"
