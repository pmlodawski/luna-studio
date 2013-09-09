---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.SSA.SSA where

import qualified Flowbox.Luna.AST.AST          as LAST
import qualified Flowbox.Luna.AST.Type         as Type
import           Flowbox.Luna.AST.Type           (Type)
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


data Mode = Write | Read

type SSAMonad m = PassMonad SSAState m


run :: PassMonad s m => LAST.Expr -> Pass.Result m LAST.Expr
run = (Pass.runM SSAState.empty) . (ssaAST Read)


runNested :: SSAMonad m => Pass.Transformer SSAState a m b 
runNested f = do
    s <- get
    Pass.runM s f


ssaAST :: SSAMonad m => Mode -> LAST.Expr -> Pass.Result m LAST.Expr
ssaAST mode ast = case ast of
    LAST.Module     cls imports classes fields 
                    methods modules     -> LAST.Module cls imports <$> ssamap classes <*> ssamap fields <*> ssamap methods <*> ssamap modules
    LAST.Function   name signature body -> runNested $ do
                                               SSAState.registerVar (name, name)
                                               ssaType signature
                                               LAST.Function name signature <$> ssamap body
    LAST.Assignment src dst             -> flip LAST.Assignment <$> ssaAST mode dst <*> ssaAST Write src
    LAST.Pattern    pat                 -> LAST.Pattern         <$> ssaAST mode pat
    LAST.Identifier name                -> case mode of
                                               Write -> LAST.Identifier <$> SSAState.handleVar name
                                               Read  -> do
                                                   v <- SSAState.lookupVar name
                                                   case v of
                                                       Nothing      -> (logger error $ "Not in scope: '" ++ name ++ "'") *> Pass.fail "Not in scope"
                                                       Just newname -> return $ LAST.Identifier newname
    LAST.Operator   name src dst        -> LAST.Operator name <$> ssaAST mode src <*> ssaAST mode dst
    LAST.Call       src args            -> LAST.Call <$> ssaAST mode src <*> ssamap args
    LAST.Class      cls classes fields 
                    methods             -> do ssaType cls
                                              LAST.Class cls <$> ssamap classes 
                                                             <*> ssamap fields 
                                                             <*> ssamap methods
    LAST.Field      {}                  -> return ast
    LAST.Constant   {}                  -> return ast
    _                                   -> logger error "SSA Pass error: Unknown expression." *> Pass.fail "Unknown expression"
    where
        ssamap = mapM (ssaAST mode)

ssaType :: SSAMonad m => Type -> Pass.Result m ()
ssaType ast = case ast of
    Type.Lambda inputs _       -> ssaType inputs
    Type.Tuple  items          -> mapM ssaType items *> return ()
    Type.Type   name           -> SSAState.registerVar (name, name)
    Type.Class  name   _       -> SSAState.registerVar (name, name)
    _                          -> logger error "SSA Pass error: Unknown type." *> Pass.fail "Unknown type"
