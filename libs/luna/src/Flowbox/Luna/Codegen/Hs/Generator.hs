---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Codegen.Hs.Generator where

import qualified Flowbox.Luna.Parser.AST.AST          as LAST
import qualified Flowbox.Luna.Parser.AST.Constant     as LConstant
import qualified Flowbox.Luna.Codegen.Hs.AST.Expr     as Expr
import           Flowbox.Luna.Codegen.Hs.AST.Expr       (Expr)
import qualified Flowbox.Luna.Codegen.Hs.AST.Constant as Constant
import qualified Flowbox.Luna.Codegen.Hs.AST.Module   as Module
import           Flowbox.Luna.Codegen.Hs.AST.Module     (Module)
import qualified Flowbox.Luna.Codegen.Hs.AST.Function as Function
import           Flowbox.Luna.Codegen.Hs.AST.Function   (Function)
import qualified Flowbox.Luna.Codegen.Hs.GenState     as GenState
import           Flowbox.Luna.Codegen.Hs.GenState       (GenState)

import           Control.Monad.State                    
import           Control.Applicative                    

import           Debug.Trace                            

import           Control.Monad.State                  
import           Control.Monad.Writer                 
import           Control.Monad.RWS                    
import           Control.Monad.Trans.Maybe              
import           Control.Monad.Trans.Either       

import           Flowbox.System.Log.Logger            
import qualified Flowbox.System.Log.LogEntry        as LogEntry

logger = getLogger "Flowbox.Luna.Codegen.Hs.Generator"

--type Generator a m r = (Functor m, MonadWriter [LogEntry.LogEntry] m) => LAST.Expr -> MaybeT m r

type Generator a m = (Functor m, Enum a, MonadState a m, MonadWriter [LogEntry.LogEntry] m)

main :: IO ()
main = do
    let 
        out = runRWS (runMaybeT testme) 0 0
    return ()

testme :: Generator a m => MaybeT m ()
testme = do return ()

--genModule :: Generator a m => LAST.Expr -> MaybeT m Module
genModule ast = case ast of
    LAST.Program body -> do
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
    --                                         mainfunc <- genFunction $ LAST.Function "main" [] body) GenState.empty
    --_                                 -> error "Unknown LUNA.AST expression"


--genFunction :: Generator a m => LAST.Expr -> MaybeT m Function
--genFunction ast = case ast of
--    LAST.Function name signature body -> Function.Function name [] <$> mapM genExpr body


genExpr :: Generator a m => LAST.Expr -> MaybeT m Expr
genExpr ast = case ast of
    LAST.Constant   cst               -> case cst of
                                             LConstant.Integer val -> return $ Expr.Constant $ Constant.Integer val
                                             _                     -> logger.critical $ "Unknown LUNA.AST expression"
    LAST.Function name signature body -> Expr.Function name <$> return [] <*> mapM genExpr body
    --LAST.Operator   name src dst -> Expr.Operator name <$> genExpr src <*> genExpr dst
    ----LAST.Identifier name         -> do
    --                                    --vname <- GenState.genVarName
    --                                    --return $ Expr.Var vname
    ----LAST.Assignment src dst      -> do
    --                                    --dst' <- genExpr dst
    --                                    --src' <- GenState.genVarName
    --                                    --GenState.registerVar src' src
    --                                    --return $ Expr.Assignment (Expr.Var src') dst' Expr.Pure
    _ -> return Expr.NOP

