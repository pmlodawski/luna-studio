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
import qualified Flowbox.Luna.Data.HAST.Extension                    as HExtension
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.GenState as GenState
import           Flowbox.Luna.Passes.Transform.HAST.HASTGen.GenState   (GenState)
import qualified Flowbox.Luna.Passes.Pass                            as Pass
import           Flowbox.Luna.Passes.Pass                              (PassMonad)
import           Flowbox.System.Log.Logger                             
import           Flowbox.Luna.Passes.Transform.HAST.HASTGen.Utils      
import           Data.String.Utils                                     (join)

import           Control.Monad.State                                 hiding (mapM, mapM_, join)
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
genModule lmod@(LModule.Module _ cls imports classes _ methods _) = do 
    let (LType.Module _ path) = cls
        mod     = HModule.addImport ["FlowboxM", "Luna", "Helpers", "Core"]
                $ HModule.addExt HExtension.TemplateHaskell
                $ HModule.addExt HExtension.MultiParamTypeClasses
                $ HModule.addExt HExtension.FlexibleInstances
                $ HModule.addExt HExtension.UndecidableInstances
                -- $ HModule.addExt HExtension.RebindableSyntax 
                $ HModule.addExt HExtension.ScopedTypeVariables
                $ HModule.mk path
        name    = last path
        modcls  = LModule.mkClass lmod
        modclss = classes ++ [modcls]
        --modclss = classes

    GenState.setModule mod
    mapM_ (genExpr ) modclss
    mapM_ (genExpr >=> GenState.addImport)   imports
    when (name == "Main") $ do
        let funcnames = map LExpr.name methods
        if not $ "main" `elem` funcnames
            then logger warning "No 'main' function defined."
            else GenState.addFunction mainf
    GenState.getModule


mainf :: HExpr
mainf = HExpr.Function "main" [] 
      $ HExpr.AppE (HExpr.Var "get0") 
      $ HExpr.AppE (HExpr.Var "_main") 
      $ HExpr.AppE (HExpr.Var "get0") 
      $ HExpr.Var "con_Main"


genfTyped arglen name ccname params base = test where
    argVars  = map (("v" ++).show) [1..arglen]
    exprVars = map HExpr.Var argVars
    selfVar  = HExpr.TypedP t $ HExpr.Var "self"
    funcVars = selfVar : exprVars
    cfGetter = foldr (flip HExpr.AppE) base exprVars
    test = HExpr.Function (mkTName arglen name) funcVars
         $ cfGetter 
    t = foldl (HExpr.AppE) (HExpr.Var ccname) (map HExpr.Var params) -- mkPure

genExpr :: GenMonad m => LExpr -> Pass.Result m HExpr
genExpr ast = case ast of
    LExpr.Var      _ name                  -> pure $ HExpr.Var $ mkVarName name
    LExpr.Cons     _ name                  -> pure $ HExpr.Var ("con" ++ mkConsName name)
    LExpr.Function _ name inputs output 
                     body                  -> do
                                              clsName <- GenState.getClsName
                                              let genf arglen mname = test where
                                                      argvars  = map (("v" ++).show) [1..arglen]
                                                      vars     = "self" : argvars
                                                      exprArgs = map HExpr.Var argvars
                                                      exprVars = map HExpr.Var vars
                                                      cfGetterBase = HExpr.AppE (HExpr.Var $ mkFuncName mname)
                                                                   $ HExpr.AppE (HExpr.Var $ mkGetName $ mkCFName mname) (HExpr.Var "self")
                                                      cfGetter = foldr (flip HExpr.AppE) cfGetterBase exprArgs
                                                      
                                                      test = HExpr.Function (mkTName arglen mname) exprVars
                                                           $ cfGetter

                                                  arglen = length inputs - 1
                                                  mname  = mangleName clsName $ mkVarName name
                                                  test   = genf arglen mname

                                                  cgetCName = mkCGetCName arglen
                                                  cgetName  = mkCGetName  arglen
                                                  getNName  = mkTName arglen mname
                                                  fname     = mkFuncName mname
                                                  f         = HExpr.Function fname <$> (mapM genExpr inputs)
                                                                                   <*> (HExpr.DoBlock <$> ((emptyHExpr :) <$> genFuncBody body output))

                                              GenState.addFunction =<< f

                                              -- GetN functions
                                              GenState.addFunction $ test

                                              -- TH snippets
                                              GenState.addTHExpression $ genTHF cgetCName getNName cgetName

                                              
                                              f

    LExpr.Arg _ pat value                  -> genPat pat
                                                  
    LExpr.Import _ path target rename      -> do
                                              let (LType.Cons _ segments) = path
                                              tname <- case target of
                                                  LExpr.Cons     _ tname -> pure tname
                                                  LExpr.Var      _ tname -> pure tname
                                                  LExpr.Wildcard _       -> logger error ("Wildcard imports are not supported yet.") *> Pass.fail "Wildcard imports are not supported yet."
                                                  _                      -> Pass.fail "internal error"
                                              case rename of
                                                  Just _                 -> logger error ("Named imports are not supported yet.") *> Pass.fail "Named imports are not supported yet."
                                                  _                      -> pure ()
                                              
                                              return $ HExpr.Import False (["FlowboxM", "Libs"] ++ segments ++ [tname]) Nothing where
                                                
    LExpr.Class _ cls _ fields methods     -> do 
                                              GenState.setClsName name
                                              let fieldNames  = map LExpr.name fields
                                                  fieldlen    = length fields
                                                  funcNames   = map LExpr.name methods 
                                                  memberNames = map mkVarName $ fieldNames ++ funcNames
                                                  cfNames     = map (mkCFName . mangleName name) memberNames
                                                  fcNames     = map mkFCName memberNames
                                                  ccname      = mkCCName name
                                            
                                              -- DataType
                                              cons   <- HExpr.Con name <$> mapM genExpr fields
                                              let dt = HExpr.DataD name params [cons] ["Show"]
                                              GenState.addDataType dt

                                              -- CF types for each class field
                                              let nts = map (genCFDec name params) cfNames
                                              mapM_ GenState.addNewType nts
                                           
                                              -- CF imports
                                              let imps = map genFCImport memberNames
                                              mapM_ GenState.addImport imps
                                           
                                              -- TH snippets
                                              let ths = zipWith3 genTHC fcNames cfNames memberNames
                                              mapM_ GenState.addTHExpression ths
                                            
                                              -- Class methods
                                              --mapM_ GenState.addFunction =<< mapM genExpr methods
                                              mapM_ genExpr methods

                                              -- CONSTRUCTORS --

                                              -- CC type constructor
                                              let con = genCCDec ccname
                                              GenState.addDataType con

                                              -- constructor function
                                              GenState.addFunction $ genCon name ccname

                                              let test   = genfTyped fieldlen name ccname [] (HExpr.ConE [name])
                                              GenState.addFunction $ test

                                              -- Constructor TH snippet
                                              let arglen    = fieldlen
                                                  cgetCName = mkCGetCName arglen
                                                  cgetName  = mkCGetName  arglen
                                                  getNName  = mkTName arglen name
                                              GenState.addTHExpression $ genTHF cgetCName getNName cgetName

                                              
                                              return dt

                                              where name   =  LType.name   cls
                                                    params =  LType.params cls
                                              
    LExpr.Infix _ name src dst             -> HExpr.Infix name <$> genExpr src <*> genExpr dst
    LExpr.Assignment _ pat dst             -> HExpr.Assignment <$> genPat pat <*> genCallExpr dst
    LExpr.Lit        _ value               -> genLit value
    LExpr.Tuple      _ items               -> HExpr.Tuple <$> mapM genExpr items -- zamiana na wywolanie funkcji!
    LExpr.Field      _ name cls _          -> genTyped HExpr.Typed cls <*> pure (HExpr.Var $ mkFieldName name)
    LExpr.App        _ src args            -> (liftM2 . foldl) HExpr.AppE (getN (length args) <$> genExpr src) (mapM genExpr args)
    LExpr.Accessor   _ src dst             -> (HExpr.AppE <$> (genExpr dst) <*> (get0 <$> genExpr src))
    LExpr.Native     _ segments            -> pure $ HExpr.Native (join " " $ map genNative segments)
    --LExpr.Native     _ segments            -> pure $ HExpr.Native code
    --_                                      -> fail $ show ast
    where
        getN n = HExpr.AppE (HExpr.Var $ "get" ++ show n)
        get0   = getN (0::Int)

genNative expr = case expr of
    LExpr.NativeCode _ code -> code
    LExpr.NativeVar  _ name -> mkVarName name

genCallExpr :: GenMonad m => LExpr -> Pass.Result m HExpr
genCallExpr e = trans <$> genExpr e where
    trans = case e of
        LExpr.Accessor {} -> ret 
        LExpr.Var      {} -> ret
        LExpr.Cons     {} -> ret 
        LExpr.Lit      {} -> ret
        _                 -> Prelude.id
    ret   = HExpr.AppE $ HExpr.Var "return"

genFuncBody :: GenMonad m => [LExpr] -> LType -> Pass.Result m [HExpr]
genFuncBody exprs output = case exprs of
    []   -> pure []
    x:[] -> liftM (:[]) $ genTyped HExpr.Typed output <*> case x of
            LExpr.Assignment _ _ dst -> genCallExpr dst 
            _                        -> genCallExpr x 
    x:xs -> (:) <$> genCallExpr x <*> genFuncBody xs output


genPat :: GenMonad m => LPat.Pat -> Pass.Result m HExpr
genPat p = case p of
    LPat.Var     _ name     -> return $ HExpr.Var (mkVarName name)
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
    _ -> fail $ show lit
    where mkLit cons hast = return $ HExpr.TypedE (HExpr.ConT cons) (HExpr.Lit hast)
          mkPure = HExpr.AppT (HExpr.ConT "pureIO")

