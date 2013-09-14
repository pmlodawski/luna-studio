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
    Expr.Module     cls imports classes fields 
                    methods modules     -> Expr.Module cls imports <$> ssaExprMap classes <*> ssaExprMap fields <*> ssaExprMap methods <*> ssaExprMap modules
    Expr.Function   name signature body -> runNested $ do
                                               SSAState.registerVar (name, name)
                                               mapM ssaPat signature
                                               Expr.Function name signature <$> ssaExprMap body
    Expr.Assignment src dst             -> flip Expr.Assignment <$> ssaAST dst <*> ssaAST src
    Expr.Pattern    pat                 -> Expr.Pattern         <$> ssaPat pat
    Expr.Var        name                -> do
                                           v <- SSAState.lookupVar name
                                           case v of
                                               Nothing    -> (logger error $ "Not in scope: '" ++ name ++ "'") *> Pass.fail "Not in scope"
                                               Just lname -> return $ Expr.Var lname
    Expr.Infix      name src dst        -> Expr.Infix name <$> ssaAST src <*> ssaAST dst
    Expr.App        src args            -> Expr.App <$> ssaAST src <*> ssaExprMap args
    Expr.Class      cls classes fields 
                    methods             -> do ssaType cls
                                              Expr.Class cls <$> ssaExprMap classes 
                                                             <*> ssaExprMap fields 
                                                             <*> ssaExprMap methods
    Expr.Field      {}                  -> return ast
    Expr.Lit        {}                  -> return ast
    _                                   -> logger error "SSA Pass error: Unknown expression." *> Pass.fail "Unknown expression"
    where
        ssaExprMap = mapM ssaAST

ssaPat :: SSAMonad m => Pat -> Pass.Result m Pat
ssaPat pat = case pat of
	Pat.Var     name                    -> Pat.Var <$> SSAState.handleVar name
	_                                   -> logger error "SSA Pass error: Unknown pattern." *> Pass.fail "Unknown pattern"

ssaType :: SSAMonad m => Type -> Pass.Result m ()
ssaType ast = case ast of
    Type.Tuple  items          -> mapM ssaType items *> return ()
    Type.Var    name           -> SSAState.registerVar (name, name)
    Type.Class  name   _       -> SSAState.registerVar (name, name)
    _                          -> logger error "SSA Pass error: Unknown type." *> Pass.fail "Unknown type"
