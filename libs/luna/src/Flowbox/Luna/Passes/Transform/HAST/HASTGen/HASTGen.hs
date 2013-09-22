---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen where

import qualified Flowbox.Prelude                                     as Prelude
import           Flowbox.Prelude                                     hiding (error, id)
import qualified Flowbox.Luna.Data.AST.Expr                          as LExpr
import qualified Flowbox.Luna.Data.AST.Type                          as LType
import qualified Flowbox.Luna.Data.AST.Pat                           as LPat
import qualified Flowbox.Luna.Data.AST.Lit                           as LLit
import qualified Flowbox.Luna.Data.AST.Module                        as LModule
import qualified Flowbox.Luna.Data.HAST.Expr                         as HExpr
import qualified Flowbox.Luna.Data.HAST.Lit                          as HLit
import qualified Flowbox.Luna.Data.HAST.Module                       as HModule
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.GenState as GenState
import           Flowbox.Luna.Passes.Transform.HAST.HASTGen.GenState   (GenState)
import qualified Flowbox.Luna.Passes.Pass                            as Pass
import           Flowbox.Luna.Passes.Pass                              (PassMonad)
import           Flowbox.System.Log.Logger           
import           Flowbox.Luna.Passes.Transform.HAST.HASTGen.Utils                  

import           Control.Monad.State                                   
import           Control.Applicative                                   

type GenMonad m = PassMonad GenState m

type HExpr   = HExpr.Expr
type LExpr   = LExpr.Expr
type LType   = LType.Type
type LModule = LModule.Module


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen"


run :: PassMonad s m => LModule -> Pass.Result m HExpr
run = (Pass.run_ (Pass.Info "HASTGen") GenState.empty) . genModule


genModule :: GenMonad m => LModule -> Pass.Result m HExpr
genModule (LModule.Module _ cls imports classes _ methods _) = do 
    let (LType.Module _ path) = cls
        mod = HModule.addImport ["Flowbox", "Gen", "Core"]
            $ HModule.mk path

    GenState.setModule mod
    mapM_ (genExpr >=> GenState.addDataType) classes
    mapM_ (genExpr >=> GenState.addImport)   imports
    mapM_ (genExpr >=> GenState.addFunction)   methods
    GenState.getModule




genExpr :: GenMonad m => LExpr -> Pass.Result m HExpr
genExpr ast = case ast of
    LExpr.Var      _ name                       -> pure $ HExpr.Var name
    LExpr.Cons     _ name                       -> pure $ HExpr.Var ("con_" ++ name)
    LExpr.Function _ name pats output body      ->     HExpr.Function name 
                                                   <$> mapM genPat pats 
                                                   <*> (HExpr.DoBlock <$> genFuncBody body output)
                                                       
    LExpr.Import _ path target rename           -> do
                                                   let (LType.Cons _ segments) = path
                                                   tname <- case target of
                                                       LExpr.Cons     _ tname -> pure tname
                                                       LExpr.Var      _ tname -> pure tname
                                                       LExpr.Wildcard _       -> logger error ("Wildcard imports are not supported yet.") *> Pass.fail "Wildcard imports are not supported yet."
                                                       _                      -> Pass.fail "internal error"
                                                   case rename of
                                                       Just _                 -> logger error ("Named imports are not supported yet.") *> Pass.fail "Named imports are not supported yet."
                                                       _                      -> pure ()

                                                   return $ HExpr.Import False (["Flowbox", "Libs"] ++ segments ++ [tname]) Nothing where
                                                     
    LExpr.Class _ cls _ fields methods          -> do 
                                                   let fieldNames  = map LExpr.name fields
                                                       funcNames   = map LExpr.name methods 
                                                       memberNames = fieldNames ++ funcNames
                                                       cfNames     = map mkCFName memberNames
                                                       fcNames     = map mkFCName memberNames

                                                   -- constructor function
                                                   GenState.addFunction $ genCon name (length fields)

                                                   -- CField for each class field
                                                   let nts = map genCFDec cfNames
                                                   mapM_ GenState.addNewType nts

                                                   -- CField imports
                                                   let imps = map genFCImport memberNames
                                                   mapM_ GenState.addImport imps

                                                   -- TH snippets
                                                   let ths = zipWith3 genTHC fcNames cfNames memberNames
                                                   mapM_ GenState.addTHExpression ths

                                                   -- HExpr generation
                                                   cons   <- HExpr.Con name <$> mapM genExpr fields
                                                   return  $ HExpr.DataType name params [cons] 
                                                     
                                                   where name   =  LType.name   cls
                                                         params =  LType.params cls
                                                   
    LExpr.Infix _ name src dst                  -> HExpr.Infix name <$> genExpr src <*> genExpr dst
    LExpr.Assignment _ pat dst                  -> HExpr.Assignment <$> genPat pat <*> genExpr dst
    LExpr.Lit        _ value                    -> genLit value
    LExpr.Tuple      _ items                    -> HExpr.Tuple <$> mapM genExpr items -- zamiana na wywolanie funkcji!
    LExpr.Field      _ name cls                 -> genTyped HExpr.Typed cls <*> pure (HExpr.Var $ mkFieldName name)
    LExpr.App        _ src args                 -> (liftM2 . foldl) HExpr.AppE (getN (length args) <$> genExpr src) (mapM genExpr args)
    LExpr.Accessor   _ src dst                  -> (HExpr.AppE <$> (genExpr dst) <*> (get0 <$> genExpr src))
    _                                            -> fail $ show ast
    where
        getN n = HExpr.AppE (HExpr.Var $ "get" ++ show n)
        get0   = getN (0::Int)

genFuncBody :: GenMonad m => [LExpr] -> LType -> Pass.Result m [HExpr]
genFuncBody exprs output = case exprs of
    []   -> pure []
    x:[] -> liftM (:[]) $ genTyped HExpr.Typed output <*> case x of
            LExpr.Assignment _ _ dst -> genExpr dst 
            _                        -> genExpr x 
    x:xs -> (:) <$> genExpr x <*> genFuncBody xs output


genPat :: GenMonad m => LPat.Pat -> Pass.Result m HExpr
genPat p = case p of
    LPat.Var     _ name     -> return $ HExpr.Var name
    LPat.Typed   _ pat cls  -> genTyped HExpr.TypedP cls <*> genPat pat
                                   

genTyped :: GenMonad m => (HExpr -> HExpr -> HExpr) -> LType -> Pass.Result m (HExpr -> HExpr)
genTyped cls t = case t of
    LType.Unknown _          -> pure Prelude.id
    _                        -> cls <$> genType t

genType :: GenMonad m => LType -> Pass.Result m HExpr
genType t = case t of
    LType.Var     _ name     -> return $ HExpr.Var (name)
    LType.Cons    _ segments -> return $ HExpr.ConE segments
    LType.Tuple   _ items    -> HExpr.Tuple <$> mapM genType items
    LType.App     _ src args -> (liftM2 . foldl) (HExpr.AppT) (genType src) (mapM genType args)
    LType.Unknown _          -> logger emergency "Cannot generate code for unknown type" *> Pass.fail "Cannot generate code for unknown type"
    _                        -> fail $ show t
    --HExpr.AppT <$> genType src <*> genType (args !! 0)

genLit :: GenMonad m => LLit.Lit -> Pass.Result m HExpr
genLit lit = case lit of
    LLit.Integer _ str      -> mkLit "Int" (HLit.Integer str)
    where mkLit cons hast = return . mkPure $ HExpr.Typed (HExpr.ConT cons) (HExpr.Lit hast)
          mkPure = HExpr.AppT (HExpr.ConT "Pure")

