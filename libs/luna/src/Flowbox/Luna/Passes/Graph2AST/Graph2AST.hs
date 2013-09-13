---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.Graph2AST.Graph2AST where

import           Flowbox.Prelude                 
import qualified Flowbox.Luna.AST.Expr         as LExpr
import qualified Flowbox.Luna.AST.Type         as Type
import           Flowbox.Luna.AST.Type           (Type)
import qualified Flowbox.Luna.AST.Constant     as LConstant
import qualified Flowbox.Luna.Passes.SSA.State as SSAState
import           Flowbox.Luna.Passes.SSA.State   (SSAState)
import qualified Flowbox.Luna.Passes.Pass      as Pass
import           Flowbox.Luna.Passes.Pass        (PassMonad)

import           Control.Monad.State             
import           Control.Applicative             

import           Debug.Trace                     

import           Control.Monad.State             
import           Control.Monad.Writer            
import           Control.Monad.RWS               
import           Control.Monad.Trans.Maybe       
import           Control.Monad.Trans.Either      
import           Data.Maybe                      (fromJust)

import qualified Flowbox.System.Log.Logger     as Logger
import           Flowbox.System.Log.Logger       
import qualified Flowbox.System.Log.LogEntry   as LogEntry

import qualified Prelude                       as Prelude
import           Prelude                       hiding (error)

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Graph2AST.Graph2AST"

type Graph2ASTMonad m = PassMonad Pass.NoState m


run :: PassMonad s m => Int -> Pass.Result m Int
run = (Pass.runM Pass.NoState) . gen



gen :: Graph2ASTMonad m => Int -> Pass.Result m Int
gen i = return 5
    