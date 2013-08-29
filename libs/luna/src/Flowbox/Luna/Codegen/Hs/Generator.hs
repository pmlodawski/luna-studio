---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Codegen.Hs.Generator where

import qualified Flowbox.Luna.Parser.AST.AST                     as LAST
import qualified Flowbox.Luna.Parser.AST.Constant                as LConstant
import qualified Flowbox.Luna.Codegen.Hs.AST.Expr                as Expr

import qualified Flowbox.Luna.Codegen.Hs.AST.Constant as Constant

import qualified Flowbox.Luna.Codegen.Hs.AST.Module     as Module
import           Flowbox.Luna.Codegen.Hs.AST.Module       (Module)
import qualified Flowbox.Luna.Codegen.Hs.AST.Function   as Function
import           Flowbox.Luna.Codegen.Hs.AST.Function     (Function)

import Debug.Trace

genModule ast = case ast of
    LAST.Program  body                ->   Module.addFunction mainfunc
                                         $ Module.empty
                                         where
                                             mainfunc = genFunction $ LAST.Function "main" [] body
    _                                 -> error "Unknown LUNA.AST expression"
    --LAST.Function name signature body -> name



genFunction ast = case ast of
    LAST.Function name signature body -> Function.empty { Function.name = name 
                                                        , Function.body = map genExpr body
                                                        }


genExpr ast = case ast of
    LAST.Constant cst          -> case cst of
                                      LConstant.Integer val -> Expr.Constant $ Constant.Integer val
                                      _                     -> error "Unknown LUNA.AST expression"
    LAST.Operator name src dst -> Expr.Operator name (genExpr src) (genExpr dst)
    _ -> Expr.NOP

