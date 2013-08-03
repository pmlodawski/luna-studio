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
    setter
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

genCode :: Function -> String
genCode func =  header  ++ signature ++ " = " ++ body ++ "\n"
             ++ headerM ++ signature ++ " = " ++ bodyM where
    signature = join " " (inputs func)
    fname  = name func 
    header = fname ++ " "
    headerM = Path.mkMonadName fname ++ " "
    body   = genBodyPure func
    bodyM  = genBodyM    func

genBodyM :: Function -> String
genBodyM func = "do\n" ++ genExprCode (exprs func, Expr.IO) ++ "    return " ++ Path.outputs ++ "\n"

genBodyPure :: Function -> String
genBodyPure func = "\n" ++ genExprCode (map Expr.mkPure $ exprs func, Expr.IO) ++ "    in " ++ Path.outputs ++ "\n"

genExprCode :: ([Expr], Expr.Context) -> String
genExprCode (exprs, ctx) = case exprs of
    []     -> ""
    x : xs -> prefix ++ indent ++ Expr.genCode x ++ "\n" ++ genExprCode (xs, ectx) where
        ectx = Expr.ctx x
        indent = case ectx of
            Expr.Pure -> Path.mkIndent 2
            _         -> Path.mkIndent 1
        prefix = if ctx == Expr.IO && ectx == Expr.Pure
            then Path.mkIndent 1 ++ "let\n"
            else ""

simple :: String -> Expr -> Function
simple name expr = Function name [Path.inputs] [expr] Expr.Pure

setCtx :: Expr.Context -> Function -> Function
setCtx nctx func = func{ctx = nctx}


addExpr :: Expr -> Function -> Function
addExpr expr func = func { exprs = expr : exprs func }


addAlias :: (String, String) -> Function -> Function
addAlias alias = addExpr (Expr.mkAlias alias)

getter :: String -> String -> String -> Expr
getter obj name param = Expr.Call "getter''" [Expr.Var obj, Expr.Var param] Expr.Pure

setter :: String -> String -> String -> Expr
setter obj name param = Expr.Call "setter''" [Expr.Var obj, Expr.Var param] Expr.Pure



