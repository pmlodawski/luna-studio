---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Transform.HAST.HASTGen.GenState where

import           Flowbox.Prelude               hiding (mod)
import           Control.Monad.State             
import qualified Flowbox.Luna.Data.HAST.Expr   as HExpr
import qualified Flowbox.Luna.Data.HAST.Module as Module

import           Flowbox.System.Log.Logger       
import           Control.Applicative             


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.HSGen.GenState"


type HExpr = HExpr.Expr

data GenState = GenState { mod :: HExpr
                         }

type GenStateM m = (MonadState GenState m, Functor m)


empty :: GenState
empty = GenState HExpr.Undefined


setModule :: GenStateM m => HExpr -> m ()
setModule m = do 
    s <- get
    put s { mod = m }


getModule :: GenStateM m => m HExpr
getModule = mod <$> get


addDataType :: GenStateM m => HExpr -> m ()
addDataType dt = do 
    m <- getModule
    setModule $ m { Module.datatypes = dt : Module.datatypes m }


addNewType :: GenStateM m => HExpr -> m ()
addNewType dt = do 
    m <- getModule
    setModule $ m { Module.newtypes = dt : Module.newtypes m }


addImport :: GenStateM m => HExpr -> m ()
addImport imp = do 
    m <- getModule
    setModule $ m { Module.imports = imp : Module.imports m }


addFunction :: GenStateM m => HExpr -> m ()
addFunction fun = do 
    m <- getModule
    setModule $ m { Module.methods = fun : Module.methods m }


addTHExpression :: GenStateM m => HExpr -> m ()
addTHExpression e = do 
    m <- getModule
    setModule $ m { Module.thexpressions = e : Module.thexpressions m }
