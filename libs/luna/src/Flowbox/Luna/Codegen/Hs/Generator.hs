---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

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



genModule ast = case ast of
    LAST.Program  body                ->   Module.addFunction mainfunc
                                         $ Module.empty
                                         where
                                             (mainfunc, _) = runState (genFunction $ LAST.Function "main" [] body) GenState.empty
    _                                 -> error "Unknown LUNA.AST expression"


genFunction :: LAST.Expr -> State GenState Function
genFunction ast = case ast of
    LAST.Function name signature body -> Function.Function name [] <$> mapM genExpr body


genExpr :: LAST.Expr -> State GenState Expr
genExpr ast = case ast of
    LAST.Constant   cst          -> return $ case cst of
                                        LConstant.Integer val -> Expr.Constant $ Constant.Integer val
                                        _                     -> error "Unknown LUNA.AST expression"
    LAST.Operator   name src dst -> Expr.Operator name <$> genExpr src <*> genExpr dst
    LAST.Identifier name         -> do
                                        vname <- GenState.genVarName
                                        return $ Expr.Var vname
    _ -> return Expr.NOP

