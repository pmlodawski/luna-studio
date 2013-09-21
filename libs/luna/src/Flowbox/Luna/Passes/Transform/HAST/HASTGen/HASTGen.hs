---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen where

import qualified Flowbox.Luna.Data.AST.Expr                          as LExpr
import qualified Flowbox.Luna.Data.AST.Type                          as LType
import qualified Flowbox.Luna.Data.AST.Pat                           as LPat
import qualified Flowbox.Luna.Data.AST.Lit                           as LLit
import qualified Flowbox.Luna.Data.AST.Module                        as LModule
import qualified Flowbox.Luna.Data.HAST.Expr                         as HExpr
import qualified Flowbox.Luna.Data.HAST.Lit                          as HLit
import qualified Flowbox.Luna.Data.HAST.Module                       as HModule
import qualified Flowbox.Luna.Data.HAST.DataType                     as DataType
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.GenState as GenState
import           Flowbox.Luna.Passes.Transform.HAST.HASTGen.GenState   (GenState)
import qualified Flowbox.Luna.Passes.Pass                            as Pass
import           Flowbox.Luna.Passes.Pass                              (PassMonad)

import           Control.Monad.State                                   
import           Control.Applicative                                   

import           Debug.Trace                                           

import           Control.Monad.State                                   
import           Control.Monad.Writer                                  
import           Control.Monad.RWS                                     
import           Control.Monad.Trans.Maybe                             
import           Control.Monad.Trans.Either                            
import           Data.Maybe                                            (fromJust)

import qualified Flowbox.System.Log.Logger                           as Logger
import           Flowbox.System.Log.Logger                             
import qualified Flowbox.System.Log.LogEntry                         as LogEntry

import qualified Flowbox.Prelude                                     as Prelude
import           Flowbox.Prelude                                     hiding (error, id)

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.HSGen.HSGen"

type GenMonad m = PassMonad GenState m

type HExpr   = HExpr.Expr
type LExpr   = LExpr.Expr
type LType   = LType.Type
type LModule = LModule.Module

run :: PassMonad s m => LModule -> Pass.Result m HExpr
run = (Pass.run_ GenState.empty) . genModule


genModule :: GenMonad m => LModule -> Pass.Result m HExpr
genModule (LModule.Module id cls imports classes fields methods modules) = do 
    let (LType.Module _ path) = cls
    GenState.setModule $ HModule.mk path
    mapM (genExpr >=> GenState.addDataType) classes
    mapM (genExpr >=> GenState.addImport)   imports
    mapM (genExpr >=> GenState.addMethod)   methods
    GenState.getModule


genExpr :: GenMonad m => LExpr -> Pass.Result m HExpr
genExpr ast = case ast of
    LExpr.Var      id name                       -> pure $ get0 (HExpr.Var name)
    LExpr.Cons     id name                       -> pure $ get0 (HExpr.Var name)
    LExpr.Function id name pats output body      ->     HExpr.Function name 
                                                    <$> mapM genPat pats 
                                                    <*> (HExpr.DoBlock <$> genFuncBody body output)
                                                       
    LExpr.Import id path target rename           -> do
                                                    let (LType.Cons _ segments) = path
                                                    tname <- case target of
                                                        LExpr.Cons     _ tname -> pure tname
                                                        LExpr.Var      _ tname -> pure tname
                                                        LExpr.Wildcard _       -> logger error ("Wildcard imports are not supported yet.") *> Pass.fail "Wildcard imports are not supported yet."
                                                        _                      -> Pass.fail "internal error"
                                                    case rename of
                                                        Just _                 -> logger error ("Named imports are not supported yet.") *> Pass.fail "Named imports are not supported yet."
                                                        _                      -> pure ()

                                                    return $ HExpr.Import False (segments ++ [tname]) Nothing where
                                                     
    LExpr.Class id cls classes fields methods    -> do 
                                                    cons   <- HExpr.Con name <$> mapM genExpr fields
                                                    return  $ HExpr.DataType name params [cons] 
                                                      
                                                    where name   =  LType.name   cls
                                                          params =  LType.params cls
                                                    
    LExpr.Infix id name src dst                  -> HExpr.Infix name <$> genExpr src <*> genExpr dst
    LExpr.Assignment id pat dst                  -> HExpr.Assignment <$> genPat pat <*> genExpr dst
    LExpr.Lit        id value                    -> genLit value
    LExpr.Tuple      id items                    -> HExpr.Tuple <$> mapM genExpr items -- zamiana na wywolanie funkcji!
    LExpr.Field      id name cls                 -> genTyped HExpr.Typed cls <*> pure (HExpr.Var name)
    LExpr.App        id src args                 -> (liftM2 . foldl) HExpr.AppE (getN (length args) <$> genExpr src) (mapM genExpr args)
    LExpr.Accessor   id src dst                  -> get0 <$> (HExpr.AppE <$> genExpr dst <*> genExpr src)
    _                                            -> fail $ show ast
    where
        getN n = HExpr.AppE (HExpr.Var $ "get" ++ show n)
        get0   = getN 0

genFuncBody :: GenMonad m => [LExpr] -> LType -> Pass.Result m [HExpr]
genFuncBody exprs output = case exprs of
    x:[] -> liftM (:[]) $ genTyped HExpr.Typed output <*> case x of
        LExpr.Assignment _ _ dst -> genExpr dst 
        _                        -> genExpr x 
    x:xs -> (:) <$> genExpr x <*> genFuncBody xs output


genPat :: GenMonad m => LPat.Pat -> Pass.Result m HExpr
genPat pat = case pat of
    LPat.Var     id name     -> return $ HExpr.Var name
    LPat.Typed   id pat cls  -> genTyped HExpr.TypedP cls <*> genPat pat
                                   

genTyped :: GenMonad m => (HExpr -> HExpr -> HExpr) -> LType -> Pass.Result m (HExpr -> HExpr)
genTyped cls t = case t of
    LType.Unknown _          -> pure Prelude.id
    _                        -> cls <$> genType t

genType :: GenMonad m => LType -> Pass.Result m HExpr
genType t = case t of
    LType.Var    id name     -> return $ HExpr.Var (name)
    LType.Cons   id segments -> return $ HExpr.ConE segments
    LType.Tuple  id items    -> HExpr.Tuple <$> mapM genType items
    LType.App    id src args -> (liftM2 . foldl) (HExpr.AppT) (genType src) (mapM genType args)
    LType.Unknown _          -> logger emergency "Cannot generate code for unknown type" *> Pass.fail "Cannot generate code for unknown type"
    _                        -> fail $ show t
    --HExpr.AppT <$> genType src <*> genType (args !! 0)

genLit :: GenMonad m => LLit.Lit -> Pass.Result m HExpr
genLit lit = case lit of
    LLit.Integer id str      -> mkLit "Int" (HLit.Integer str)
    where mkLit cons hast = return . mkPure $ HExpr.Typed (HExpr.ConT cons) (HExpr.Lit hast)
          mkPure = HExpr.AppT (HExpr.ConT "Pure")

