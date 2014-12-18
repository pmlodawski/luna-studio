---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.Pass2.Target.HS.HASTGen.State where

import Control.Monad.State

import           Flowbox.Prelude       hiding (mod)
import qualified Luna.AST.Expr         as LExpr
import qualified Luna.AST.Type         as LType
import qualified Luna.Data.HAST.Expr   as HExpr
import qualified Luna.Data.HAST.Module as Module
import           Luna.Data.HAST.Comment (Comment)

import Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger $(moduleName)


type HExpr = HExpr.Expr
type LExpr = LExpr.Expr
--type LType = LType.Type

data GenState = GenState { _mod :: HExpr
                         , _ctx :: [Text]
                         }

makeLenses ''GenState

type GenStateM m = (Applicative m, MonadState GenState m, Functor m)


--empty :: GenState
--empty = GenState HExpr.Undefined (LType.Unknown 0)

popList []     = Nothing
poplist (x:xs) = Just (x,xs)

pushCtx :: GenStateM m => Text -> m()
pushCtx c = modify (ctx %~ (c:))

popCtx :: GenStateM m => m (Maybe Text)
popCtx = do
    s <- get
    let stack = view ctx s
    case stack of
        []     -> return Nothing
        (x:xs) -> put (s & ctx .~ xs) *> return (Just x)

getCtxStack :: GenStateM m => m [Text]
getCtxStack = view ctx <$> get

getCtx :: GenStateM m => m (Maybe Text)
getCtx = do
    stack <- getCtxStack
    case stack of
        []     -> return Nothing
        (x:xs) -> return $ Just x

withCtx :: GenStateM m => Text -> m a -> m a
withCtx name m = pushCtx name *> m <* popCtx 


setModule :: GenStateM m => HExpr -> m ()
setModule m = modify (mod .~ m)


getModule :: GenStateM m => m HExpr
getModule = view mod <$> get


addDataType :: GenStateM m => HExpr -> m ()
addDataType dt = do
    m <- getModule
    setModule $ m { Module._body = Module._body m ++ [dt]}


addInstance :: GenStateM m => HExpr -> m ()
addInstance inst = do
    m <- getModule
    setModule $ m { Module._body = Module._body m ++ [inst]}


addNewType :: GenStateM m => HExpr -> m ()
addNewType dt = do
    m <- getModule
    setModule $ m { Module._body = Module._body m ++ [dt] }


addTypeAlias :: GenStateM m => HExpr -> m ()
addTypeAlias el = do
    m <- getModule
    setModule $ m { Module._body = Module._body m ++ [el] }


addTypeDef :: GenStateM m => HExpr -> m ()
addTypeDef el = do
    m <- getModule
    setModule $ m { Module._body = Module._body m ++ [el] }


addImport :: GenStateM m => HExpr -> m ()
addImport imp = do
    m <- getModule
    setModule $ m { Module._imports = imp : Module._imports m }


regFunc :: GenStateM m => HExpr -> m ()
regFunc fun = do
    m <- getModule
    setModule $ m { Module._body = Module._body m ++ [fun] }

addComment :: GenStateM m => Comment -> m ()
addComment c = do
    let expr = HExpr.Comment c
    m <- getModule
    setModule $ m { Module._body = Module._body m ++ [expr] }


regTHExpr :: GenStateM m => HExpr -> m ()
regTHExpr e = do
    m <- getModule
    setModule $ m { Module._body = Module._body m ++ [e] }



instance Default GenState where
    def = GenState HExpr.Undefined def