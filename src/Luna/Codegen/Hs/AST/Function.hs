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
    --addExpr,
    --addAlias,
    setCtx,
    setBody,
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
                           body       :: Expr,
                           ctx        :: Expr.Context
                         } deriving (Show)


empty :: Function
empty = Function "" [] (Expr.mkBlock Path.outputs) Expr.Pure


basic :: Function
basic = empty { signature = [Expr.Var Path.inputs] }


genCode :: Function -> String
genCode func =  head' ++ " = " ++ body' ++ "\n"
             ++ headM ++ " = " ++ bodyM where
    fname    = name func 
    header   = fname ++ " "
    headerM  = Path.mkMonadName fname ++ " "
    inputs   = join " " (map Expr.genCode $ signature func)
    head'    = header  ++ inputs
    headM    = headerM ++ inputs
    body'    = genBodyPure func
    bodyM    = case ctx func of
                   Expr.IO   -> genBodyM func
                   Expr.Pure -> "return $ " ++ head'


genBodyM :: Function -> String
genBodyM func = "do\n" ++ Expr.genCode (body func) -- ++ "    return " ++ Path.outputs ++ "\n"

genBodyPure :: Function -> String
genBodyPure func = "\n" ++ Expr.genCode (Expr.mkPure $ body func) -- ++ "    in " ++ Path.outputs ++ "\n"


--simple :: String -> Expr -> Function
--simple name' expr = Function name' [Path.signature] [expr] Expr.Pure

setCtx :: Expr.Context -> Function -> Function
setCtx nctx func = func{ctx = nctx}


setBody :: Expr -> Function -> Function
setBody expr func = func { body = expr }

--addExpr :: Expr -> Function -> Function
--addExpr expr func = func { body = expr : body func }


--addAlias :: (String, String) -> Function -> Function
--addAlias alias = addExpr (Expr.mkAlias alias)


getter :: String -> String -> String -> Expr
getter obj _ param = Expr.Call "getter''" [Expr.THTypeCtx obj, Expr.THExprCtx param] Expr.Pure


setter :: String -> String -> String -> Expr
setter obj _ param = Expr.Call "setter''" [Expr.THTypeCtx obj, Expr.THExprCtx param] Expr.Pure



--mkSpec name' spec basefunc = Function.empty { name      = name'
--                                            , signature = [Expr.At Path.inputs (Expr.Tuple [spec, Expr.Any])]
--                                            }