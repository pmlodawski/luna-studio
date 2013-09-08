---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.HSGen.GenState where

import           Flowbox.Prelude                      hiding(mod)                  
import           Control.Monad.State                    
import qualified Data.Map                             as Map
import           Data.Map                               (Map)
import qualified Flowbox.Luna.Passes.HSGen.AST.Expr   as HAST
import qualified Flowbox.Luna.Passes.HSGen.AST.Module as Module

import           Flowbox.System.Log.Logger            


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.HSGen.GenState"


data GenState = GenState { mod :: HAST.Expr
                         }

type GenStateM m = MonadState GenState m


empty :: GenState
empty = GenState HAST.Undefined


setModule :: GenStateM m => HAST.Expr -> m ()
setModule m = do s <- get
                 put s { mod = m }


getModule :: GenStateM m => m HAST.Expr
getModule = do s <- get
               return $ mod s



addDataType :: GenStateM m => HAST.Expr -> m ()
addDataType dt = do m <- getModule
                    setModule $ m { Module.datatypes = dt : Module.datatypes m }