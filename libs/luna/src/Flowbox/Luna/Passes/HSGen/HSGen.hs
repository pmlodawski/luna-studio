---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.HSGen.HSGen where

import           Flowbox.Prelude                          
import qualified Flowbox.Luna.AST.Expr                  as LExpr
import qualified Flowbox.Luna.AST.Type                  as LType
import qualified Flowbox.Luna.AST.Pat                   as LPat
import qualified Flowbox.Luna.Passes.HSGen.AST.Expr     as HExpr
import qualified Flowbox.Luna.Passes.HSGen.AST.Constant as Constant
import qualified Flowbox.Luna.Passes.HSGen.AST.Module   as Module
import qualified Flowbox.Luna.Passes.HSGen.AST.DataType as DataType
import qualified Flowbox.Luna.Passes.HSGen.AST.Cons     as Cons
import qualified Flowbox.Luna.Passes.HSGen.GenState     as GenState
import           Flowbox.Luna.Passes.HSGen.GenState       (GenState)
import qualified Flowbox.Luna.Passes.Pass               as Pass
import           Flowbox.Luna.Passes.Pass                 (PassMonad)

import           Control.Monad.State                      
import           Control.Applicative                      

import           Debug.Trace                              

import           Control.Monad.State                      
import           Control.Monad.Writer                     
import           Control.Monad.RWS                        
import           Control.Monad.Trans.Maybe                
import           Control.Monad.Trans.Either               
import           Data.Maybe                               (fromJust)

import qualified Flowbox.System.Log.Logger              as Logger
import           Flowbox.System.Log.Logger                
import qualified Flowbox.System.Log.LogEntry            as LogEntry

import qualified Prelude                                as Prelude
import           Prelude                                hiding (error)

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.HSGen.HSGen"

type GenMonad m = PassMonad GenState m




run :: PassMonad s m => LExpr.Expr -> Pass.Result m HExpr.Expr
run = (Pass.runM GenState.empty) . genModule


genModule :: GenMonad m => LExpr.Expr -> Pass.Result m HExpr.Expr
genModule ast = case ast of
    LExpr.Module cls imports classes fields methods modules -> do 
                                                               GenState.setModule Module.empty
                                                               mapM (genExpr >=> GenState.addDataType) classes
                                                               mapM (genExpr >=> GenState.addImport)   imports
                                                               mapM (genExpr >=> GenState.addMethod)   methods
                                                               GenState.getModule
    _                                                      -> fail "o nie"


genExpr :: GenMonad m => LExpr.Expr -> Pass.Result m HExpr.Expr
genExpr ast = case ast of
    --LExpr.Module     cls classes fields
    --                methods modules     -> do GenState.setModule Module.empty
    --                                          mapM genExpr classes *> return ()
                         
    --LExpr.Constant   cst                 -> case cst of
    --                                           LConstant.Integer val -> return $ HExpr.Constant $ Constant.Integer val
    --                                           _                     -> logger criticalFail "Unknown LUNA.AST HExpression"
    LExpr.Var name                       -> return $ HExpr.Var (name)
                                 
    LExpr.Function   name signature body -> do
                                           body'  <- mapM genExpr body
                                           HExpr.Function name <$> mapM genPat signature <*> pure (HExpr.LetBlock body' HExpr.NOP)
                                            --HExpr.Function name <$> return [] <*> mapM genExpr body

    --LExpr.Import segments name           -> 
    --LExpr.Class  cls classes fields 
    --            methods                 -> do
    --                                       efields <- mapM genField fields
    --                                       let name = Type.name cls
    --                                           cons = Cons.empty { HExpr.name   = name
    --                                                             , HExpr.fields = efields
    --                                                             }
    --                                       return $ DataType.empty { HExpr.name         = name
    --                                                               , HExpr.params       = Type.params cls
    --                                                               , HExpr.constructors = [cons]
    --                                                               }  

    LExpr.Import segments name           -> return $ HExpr.Import segments name

    LExpr.Class  cls classes fields 
                methods                 -> do cons   <- HExpr.Cons name <$> mapM genField fields
                                              return $  HExpr.DataType name params [cons] 
                                              where name   =  LType.name   cls
                                                    params =  LType.params cls

    LExpr.Infix name src dst            -> HExpr.Operator name <$> genExpr src <*> genExpr dst



genPat :: GenMonad m => LPat.Pat -> Pass.Result m HExpr.Expr
genPat pat = case pat of
    LPat.Var    name             -> return $ HExpr.Var name
                                            
genType :: GenMonad m => LType.Type -> Pass.Result m HExpr.Expr
genType t = case t of
    LType.Var    name             -> return $ HExpr.Var (name)
    LType.Tuple  items            -> HExpr.Tuple <$> mapM genType items
    --Type.Lambda inputs outputs   -> do
    --                                inputs' <- HExpr.items <$> genType inputs
    --                                return $ HExpr.Function "" inputs' (HExpr.NOP)

genField :: GenMonad m => LExpr.Expr -> Pass.Result m HExpr.Expr
genField (LExpr.Field name t) = return $ HExpr.Typed (LType.name t) (HExpr.Var name)



    -- Class name params []
    --LExpr.Operator   name src dst -> HExpr.Operator name <$> genExpr src <*> genExpr dst
    ----LExpr.Identifier name         -> do
    --                                    --vname <- SSAState.genVarName
    --                                    --return $ HExpr.Var vname
    ----LExpr.Assignment src dst      -> do
    --                                    --dst' <- genExpr dst
    --                                    --src' <- SSAState.genVarName
    --                                    --SSAState.registerVar src' src
    --                                    --return $ HExpr.Assignment (HExpr.Var src') dst' HExpr.Pure
    --_ -> return HExpr.NOP




----data X a b c = X{a::a,b::b,c::c} | Y

