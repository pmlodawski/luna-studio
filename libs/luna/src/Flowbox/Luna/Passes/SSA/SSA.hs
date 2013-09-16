---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.SSA.SSA where

import qualified Flowbox.Luna.AST.Expr        as Expr
import qualified Flowbox.Luna.AST.Type        as Type
import           Flowbox.Luna.AST.Type          (Type)
import qualified Flowbox.Luna.AST.Pat         as Pat
import           Flowbox.Luna.AST.Pat           (Pat)
import qualified Flowbox.Luna.Passes.Pass     as Pass
import           Flowbox.Luna.Passes.Pass       (PassMonad)

import           Control.Monad.State            
import           Control.Applicative            

import           Flowbox.System.Log.Logger      

import           Flowbox.Prelude              hiding (error, id)



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.SSA.SSA"


type SSAMonad m = PassMonad Pass.NoState m


run :: PassMonad s m => Expr.Expr -> Pass.Result m Expr.Expr
run = (Pass.run_ Pass.NoState) . ssaExpr


ssaExpr :: SSAMonad m => Expr.Expr -> Pass.Result m Expr.Expr
ssaExpr ast = case ast of
    Expr.Var        id name               -> return $ Expr.Var id ("v_" ++ show id)
    _                                     -> Expr.traverseM ssaExpr ast