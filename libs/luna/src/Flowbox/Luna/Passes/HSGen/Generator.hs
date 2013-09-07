---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.HSGen.Generator where

import           Flowbox.Prelude
import qualified Flowbox.Luna.AST.AST                   as LAST
import qualified Flowbox.Luna.AST.Type                  as Type
import           Flowbox.Luna.AST.Type                    (Type)
import qualified Flowbox.Luna.AST.Constant              as LConstant
import qualified Flowbox.Luna.Passes.HSGen.AST.Expr     as Expr
import           Flowbox.Luna.Passes.HSGen.AST.Expr       (Expr)
import qualified Flowbox.Luna.Passes.HSGen.AST.Constant as Constant
import qualified Flowbox.Luna.Passes.HSGen.AST.Module   as Module
import qualified Flowbox.Luna.Passes.HSGen.AST.DataType as DataType
import qualified Flowbox.Luna.Passes.HSGen.AST.Function as Function
import qualified Flowbox.Luna.Passes.HSGen.AST.Cons     as Cons
import           Flowbox.Luna.Passes.HSGen.AST.Function   (Function)
import qualified Flowbox.Luna.Passes.SSA.State          as SSAState
import           Flowbox.Luna.Passes.SSA.State            (SSAState)
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
logger = getLogger "Flowbox.Luna.Passes.HSGen.Generator"

type GenMonad m = PassMonad Pass.NoState m

--run :: PassMonad s m => LAST.Expr -> Pass.Result m Expr
run = (Pass.runM Pass.NoState) . genModule


--parse :: ParserMonad m => Source -> Pass.Result m LAST.Expr
--parse (Source.Source mod code) = case Parser.parse code of
--    Left  _ -> Pass.fail
--    Right v -> return v



--genModule :: Generator a m => LAST.Expr -> MaybeT m Module
genModule ast = case ast of
    LAST.Module path body -> do
                         x <- mapM genExpr body
                         return x
                         --logger.debug $ "debug"
                         --mainfunc <- genFunction $ (LAST.Function "main" [] body)
                         --return $ Module.addFunction mainfunc
                         --       $ Module.empty
                            
    --_                 -> logger.critical $ "Unknown LUNA.AST expression"

    --n <- get
    --logger.debug $ "o nie"
    ----left "err"
    --fail "oh no"
    --put $ succ n
    --return ()

--genModule ast = 
    --case ast of
    --LAST.Program  body                ->   Module.addFunction mainfunc
    --                                     $ Module.empty
    --                                     where
    --                                         mainfunc <- genFunction $ LAST.Function "main" [] body) SSAState.empty
    --_                                 -> error "Unknown LUNA.AST expression"


--genFunction :: Generator a m => LAST.Expr -> MaybeT m Function
--genFunction ast = case ast of
--    LAST.Function name signature body -> Function.Function name [] <$> mapM genExpr body


--genDataType :: Generator a m => LAST.Expr -> MaybeT m Expr
--genDataType expr = case expr of
--    LAST.Typed t (LAST.Identifier ident) -> 


genExpr :: GenMonad m => LAST.Expr -> Pass.Result m Expr
genExpr ast = case ast of
    LAST.Constant   cst                 -> case cst of
                                               LConstant.Integer val -> return $ Expr.Constant $ Constant.Integer val
                                               _                     -> logger criticalFail "Unknown LUNA.AST expression"
    LAST.Identifier name                -> return $ Expr.Var ("v''" ++ name)

    LAST.Function   name signature body -> do
                                           lambda <- genType signature
                                           body'  <- mapM genExpr body
                                           return $ lambda { Expr.name = name
                                                           , Expr.body = body'
                                                           }
                                            --Expr.Function name <$> return [] <*> mapM genExpr body
    LAST.Class      cls fields methods  -> do
                                           efields <- mapM genField fields
                                           let name = Type.name cls
                                               cons = Cons.empty { Expr.name   = name 
                                                                 , Expr.fields = efields
                                                                 }
                                           return $ DataType.empty { Expr.name         = name
                                                                   , Expr.params       = Type.params cls
                                                                   , Expr.constructors = [cons]
                                                                   }  
                                            
genType :: GenMonad m => Type -> Pass.Result m Expr
genType t = case t of
    Type.Type   name             -> return $ Expr.Var ("v''" ++ name)
    Type.Tuple  items            -> Expr.Tuple <$> mapM genType items
    Type.Lambda inputs outputs   -> do
                                    inputs'        <- Expr.items <$> genType inputs
                                    return $ Expr.Function "" inputs' []

genField :: GenMonad m => LAST.Expr -> Pass.Result m Expr
genField (LAST.Field name t) = return $ Expr.Typed (Type.name t) (Expr.Var name)



    -- Class name params []
    --LAST.Operator   name src dst -> Expr.Operator name <$> genExpr src <*> genExpr dst
    ----LAST.Identifier name         -> do
    --                                    --vname <- SSAState.genVarName
    --                                    --return $ Expr.Var vname
    ----LAST.Assignment src dst      -> do
    --                                    --dst' <- genExpr dst
    --                                    --src' <- SSAState.genVarName
    --                                    --SSAState.registerVar src' src
    --                                    --return $ Expr.Assignment (Expr.Var src') dst' Expr.Pure
    --_ -> return Expr.NOP




----data X a b c = X{a::a,b::b,c::c} | Y

