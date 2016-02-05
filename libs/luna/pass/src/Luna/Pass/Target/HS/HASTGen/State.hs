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

module Luna.Pass.Target.HS.HASTGen.State where

import           Control.Monad.State        hiding (withState)

import           Flowbox.Prelude            hiding (mod)
import           Luna.Syntax.Name.Path      (QualPath)
import           Luna.Target.HS.AST.Comment (Comment)
import qualified Luna.Target.HS.AST.Expr    as HExpr
import qualified Luna.Target.HS.AST.Module  as Module

import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger $moduleName


type HExpr = HExpr.Expr

data GenState = GenState { _mod    :: HExpr
                         , _ctx    :: [QualPath]
                         , _callID :: Int
                         }

makeLenses ''GenState

type GenStateM m = (Applicative m, MonadState GenState m, Functor m)


--empty :: GenState
--empty = GenState HExpr.Undefined (LType.Unknown 0)

popList []     = Nothing
poplist (x:xs) = Just (x,xs)

genCallID = do
    s <- get
    let cid = view callID s
    put $ s & callID .~ cid + 1
    return cid

withState f = do
    s <- get
    put $ f s

pushCtx :: GenStateM m => QualPath -> m()
pushCtx c = modify (ctx %~ (c:))

popCtx :: GenStateM m => m (Maybe QualPath)
popCtx = do
    s <- get
    let stack = view ctx s
    case stack of
        []     -> return Nothing
        (x:xs) -> put (s & ctx .~ xs) *> return (Just x)

getCtxStack :: GenStateM m => m [QualPath]
getCtxStack = view ctx <$> get

getCtx :: GenStateM m => m (Maybe QualPath)
getCtx = do
    stack <- getCtxStack
    case stack of
        []     -> return Nothing
        (x:xs) -> return $ Just x

withCtx :: GenStateM m => QualPath -> m a -> m a
withCtx name m = pushCtx name *> m <* popCtx

setModule :: GenStateM m => HExpr -> m ()
setModule m = modify (mod .~ m)

getModule :: GenStateM m => m HExpr
getModule = view mod <$> get

withModule :: GenStateM m => (HExpr -> HExpr) -> m ()
withModule f = do
    m <- getModule
    setModule $ f m

appendModuleBody :: GenStateM m => HExpr -> m ()
appendModuleBody a = withModule (\m -> m { Module._body = Module._body m ++ [a] } )

addDataType :: GenStateM m => HExpr -> m ()
addDataType = appendModuleBody

addInstance :: GenStateM m => HExpr -> m ()
addInstance = appendModuleBody

addNewType :: GenStateM m => HExpr -> m ()
addNewType = appendModuleBody

addTypeAlias :: GenStateM m => HExpr -> m ()
addTypeAlias = appendModuleBody

addTypeDef :: GenStateM m => HExpr -> m ()
addTypeDef = appendModuleBody

addImport :: GenStateM m => HExpr -> m ()
addImport imp = do
    m <- getModule
    setModule $ m { Module._imports = imp : Module._imports m }

regFunc :: GenStateM m => HExpr -> m ()
regFunc = appendModuleBody

regDecl :: GenStateM m => HExpr -> m ()
regDecl = appendModuleBody

regPragma :: GenStateM m => HExpr -> m ()
regPragma = appendModuleBody

addComment :: GenStateM m => Comment -> m ()
addComment c = do
    let expr = HExpr.Comment c
    m <- getModule
    setModule $ m { Module._body = Module._body m ++ [expr] }

regTHExpr :: GenStateM m => HExpr -> m ()
regTHExpr = appendModuleBody


instance Default GenState where
    def = GenState HExpr.Undefined def 0
