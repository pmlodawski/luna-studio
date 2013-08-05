---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Function (
    Function(..),
    empty,
    basic,
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
import qualified Luna.Codegen.Hs.Path            as Path


data Function = Function { name       :: String,
                           signature  :: [Expr],
                           exprs      :: [Expr],
                           ctx        :: Expr.Context
                         } deriving (Show)


empty :: Function
empty = Function "" [] [] Expr.Pure


basic :: Function
basic = empty { signature = [Expr.Var Path.inputs] }


genCode :: Function -> String
genCode func =  head' ++ " = " ++ body ++ "\n"
             ++ headM ++ " = " ++ bodyM where
    inputs    = join " " (map Expr.genCode $ signature func)
    fname     = name func 
    head'     = header  ++ inputs
    headM     = headerM ++ inputs
    header    = fname ++ " "
    headerM   = Path.mkMonadName fname ++ " "
    body      = genBodyPure func
    bodyM     = case ctx func of
                    Expr.IO   -> genBodyM func
                    Expr.Pure -> "return $ " ++ head'

genBodyM :: Function -> String
genBodyM func = "do\n" ++ genExprCode (exprs func, Expr.IO) ++ "    return " ++ Path.outputs ++ "\n"

genBodyPure :: Function -> String
genBodyPure func = "\n" ++ genExprCode (map Expr.mkPure $ exprs func, Expr.IO) ++ "    in " ++ Path.outputs ++ "\n"

genExprCode :: ([Expr], Expr.Context) -> String
genExprCode (exprs', ctx') = case exprs' of
    []     -> ""
    x : xs -> prefix ++ indent ++ Expr.genCode x ++ "\n" ++ genExprCode (xs, ectx) where
        ectx = Expr.ctx x
        indent = case ectx of
            Expr.Pure -> Path.mkIndent 2
            _         -> Path.mkIndent 1
        prefix = if ctx' == Expr.IO && ectx == Expr.Pure
            then Path.mkIndent 1 ++ "let\n"
            else ""

--simple :: String -> Expr -> Function
--simple name' expr = Function name' [Path.signature] [expr] Expr.Pure

setCtx :: Expr.Context -> Function -> Function
setCtx nctx func = func{ctx = nctx}


addExpr :: Expr -> Function -> Function
addExpr expr func = func { exprs = expr : exprs func }


addAlias :: (String, String) -> Function -> Function
addAlias alias = addExpr (Expr.mkAlias alias)


getter :: String -> String -> String -> Expr
getter obj _ param = Expr.Call "getter''" [Expr.THTypeCtx obj, Expr.THExprCtx param] Expr.Pure


setter :: String -> String -> String -> Expr
setter obj _ param = Expr.Call "setter''" [Expr.THTypeCtx obj, Expr.THExprCtx param] Expr.Pure



