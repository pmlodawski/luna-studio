---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Function (
    Function(..),
    empty,
    genCode,
    addExpr,
    addAlias,
    setCtx,
    getter,
    setter,
    getterM,
    setterM
)where

import Debug.Trace


import qualified Luna.Codegen.Hs.AST.Expr        as Expr
import           Luna.Codegen.Hs.AST.Expr          (Expr)
import           Data.String.Utils                 (join)
import qualified Luna.Codegen.Hs.GenState        as GenState
import           Luna.Codegen.Hs.GenState          (GenState)
import qualified Luna.Codegen.Hs.Path            as Path


data Function = Function { name       :: String,
                           inputs     :: [String],
                           exprs      :: [Expr],
                           ctx        :: Expr.Context
                         } deriving (Show)


empty :: Function
empty = Function "" [] [] Expr.Pure

--genCode :: GenContext -> Function -> String
genCode func =  header ++ begin ++ body where
    header = name func ++ " " ++ join " " (inputs func)
    begin  = case ctx func of
                 Expr.IO -> " = do\n"
                 _       -> " = \n"
    body =  "    let\n"
         ++ join "\n" (map Expr.genCode (exprs func)) 


simple :: String -> Expr -> Function
simple name expr = Function name [Path.inputs] [expr] Expr.Pure

setCtx :: Expr.Context -> Function -> Function
setCtx nctx func = func{ctx = nctx}


addExpr :: Expr -> Function -> Function
addExpr expr func = func { exprs = expr : exprs func }


addAlias :: (String, String) -> Function -> Function
addAlias alias = addExpr (Expr.mkAlias alias)

getter :: String -> String -> String -> Expr
getter obj name param = Expr.Call "getter" [Expr.Var obj, Expr.Var param] Expr.Pure

setter :: String -> String -> String -> Expr
setter obj name param = Expr.Call "setter" [Expr.Var obj, Expr.Var param] Expr.Pure


getterM :: String -> String -> String -> Expr
getterM obj name param = Expr.Call "getter''M" [Expr.Var obj, Expr.Var param] Expr.Pure

setterM :: String -> String -> String -> Expr
setterM obj name param = Expr.Call "setter''M" [Expr.Var obj, Expr.Var param] Expr.Pure
